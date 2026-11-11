;;; scooter-auth.el --- Auth for Scooter behind oauth2-proxy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Chad Crawford
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Backends, chosen per server by its :auth setting:
;;
;; - `cookie': the `_oauth2_proxy' session cookie, pasted or read from
;;   auth-source, and saved there.  Works with any oauth2-proxy, GitHub-backed
;;   ones included.
;; - (cookie :browser BROWSER): the same cookie read from a browser's jar
;;   (`scooter-auth-browsers'; t for all), signing in there when needed.
;; - Either form takes :cookie-name NAME to match oauth2-proxy's
;;   --cookie-name (default `_oauth2_proxy').
;; - (oidc :issuer URL :client-id ID): an ID token from Authorization Code +
;;   PKCE with a loopback redirect, sent as a bearer token.  oauth2-proxy
;;   must run with --skip-jwt-bearer-tokens.  The refresh token is kept in
;;   auth-source.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'auth-source)
(require 'browse-url)
(require 'url-util)
(require 'scooter-server)
(require 'scooter-http)

(defcustom scooter-auth-login-timeout 300
  "Seconds to wait for a browser login to finish."
  :type 'integer
  :group 'scooter)

(declare-function secrets-search-items "secrets")
(declare-function secrets-delete-item "secrets")

(defvar scooter-auth--cache (make-hash-table :test #'eq)
  "Server name to its auth state plist.")

(defun scooter-auth--backend (server)
  "Return SERVER's backend symbol and its settings, as (TYPE . PLIST)."
  (let ((auth (scooter-server-get server :auth)))
    (if (consp auth) auth (list auth))))

(defconst scooter-auth-default-cookie-name "_oauth2_proxy"
  "The session cookie name oauth2-proxy uses without --cookie-name.")

(defun scooter-auth--cookie-name (server)
  "Return the name of SERVER's oauth2-proxy session cookie.
The :cookie-name in SERVER's `cookie' auth settings, else the default."
  (or (plist-get (cdr (scooter-auth--backend server)) :cookie-name)
      scooter-auth-default-cookie-name))

(defun scooter-auth-headers (server &optional retry)
  "Return the auth headers for SERVER as an alist.
RETRY non-nil means the last headers hit the auth wall."
  (when retry (scooter-auth-forget server))
  (pcase (scooter-auth--backend server)
    (`(nil) nil)
    (`(cookie . ,_) `(("Cookie" . ,(concat (scooter-auth--cookie-name server) "="
                                           (scooter-auth--cookie server)))))
    (`(oidc . ,opts) `(("Authorization" . ,(concat "Bearer " (scooter-auth--id-token server opts)))))
    (other (user-error "Scooter[%s]: unknown :auth %S" server other))))

;;;###autoload
(defun scooter-auth-headers-json (server &optional retry)
  "Return SERVER's auth headers as a JSON string, for `scooter acp'.
RETRY is as in `scooter-auth-headers'.  While a sign-in for SERVER is
already waiting on the user, return \"null\": the bridge asks again
shortly instead of opening a second prompt or failing its request."
  (let ((json-encoding-pretty-print nil))
    (condition-case nil
        (let ((headers (scooter-auth-headers
                        server (and retry (not (scooter-auth--get server :signing-in))))))
          (if headers (json-encode headers) "{}"))
      (scooter-auth-busy "null"))))

(defun scooter-auth-forget (server)
  "Drop SERVER's cached ID token and cookie; the next request logs in again."
  (interactive (list (scooter-current-server t)))
  (scooter-auth--put server :id-token nil :cookie nil :cookie-refused t))

(defun scooter-auth--get (server key)
  "Return SERVER's cached auth KEY."
  (plist-get (gethash (scooter-server--name server) scooter-auth--cache) key))

(defun scooter-auth--put (server &rest kvs)
  "Store KVS in SERVER's auth cache."
  (let ((state (gethash (scooter-server--name server) scooter-auth--cache)))
    (while kvs
      (setq state (plist-put state (pop kvs) (pop kvs))))
    (puthash (scooter-server--name server) state scooter-auth--cache)))

;;;; Cookie

(defun scooter-auth--cookie (server)
  "Return SERVER's oauth2-proxy cookie, asking when there is none."
  (or (scooter-auth--get server :cookie)
      ;; Once refused, a stored cookie is stale until replaced.
      (when-let* (((not (scooter-auth--get server :cookie-refused)))
                  (stored (scooter-auth--secret (scooter-server-host server)
                                                (scooter-auth--cookie-name server))))
        (scooter-auth--put server :cookie stored)
        stored)
      (progn
        (when scooter-auth-quiet
          (signal 'scooter-auth-needed (list (format "Scooter[%s]: not signed in" server))))
        (when-let* ((why (scooter-auth--get server :sign-in-failed)))
          (user-error "Scooter[%s]: %s; `scooter-auth-browser-login' tries again" server why))
        (scooter-auth--signing-in server (if (plist-get (cdr (scooter-auth--backend server)) :browser)
                                             #'scooter-auth--browser-sign-in
                                           #'scooter-auth-set-cookie)))))

(define-error 'scooter-auth-busy "Already signing in" 'user-error)

(defun scooter-auth--signing-in (server sign-in)
  "Call SIGN-IN with SERVER, refusing to nest a second one.
Another shell's bridge asking for headers while the prompt is open runs
inside it (emacsclient is served from the prompt's input loop), so it
signals `scooter-auth-busy' rather than open a second prompt;
`scooter-auth-headers-json' turns that into a \"try again\" answer."
  (when (scooter-auth--get server :signing-in)
    (signal 'scooter-auth-busy
            (list (format "Scooter[%s]: already signing in; finish that first" server))))
  (scooter-auth--put server :signing-in t)
  (unwind-protect (funcall sign-in server)
    (scooter-auth--put server :signing-in nil)))

;;;###autoload
(defun scooter-auth-set-cookie (server &optional cookie)
  "Set SERVER's oauth2-proxy session COOKIE, opening the login page to get one."
  (interactive (list (scooter-current-server t)))
  (let ((name (scooter-auth--cookie-name server)))
    (unless cookie
      (when (y-or-n-p (format "Scooter[%s]: open the login page in a browser? " server))
        (browse-url (scooter-server-url server "/oauth2/sign_in")))
      (setq cookie (string-trim (read-passwd
                                 (format "Scooter[%s] %s cookie value: " server name)))))
    (setq cookie (string-remove-prefix (concat name "=") cookie))
    (scooter-auth--put server :cookie cookie :cookie-refused nil)
    (scooter-auth--save-secret (scooter-server-host server) name cookie)
    cookie))

;;;; Browser sign-in

;; Two jar formats: Mozilla's cookies.sqlite and Chromium's encrypted
;; Cookies (see `scooter-auth--chromium-decrypt').

(defcustom scooter-auth-browsers
  `((firefox :jar mozilla
             :directories ("~/.mozilla/firefox"
                           ,@(when-let* ((xdg (getenv "XDG_CONFIG_HOME")))
                               (list (expand-file-name "mozilla/firefox" xdg)))
                           "~/.config/mozilla/firefox"
                           "~/Library/Application Support/Firefox"
                           "~/snap/firefox/common/.mozilla/firefox"
                           "~/.var/app/org.mozilla.firefox/.mozilla/firefox")
             :program browse-url-firefox-program)
    (librewolf :jar mozilla
               :directories ("~/.librewolf"
                             "~/.var/app/io.gitlab.librewolf-community/.librewolf"
                             "~/Library/Application Support/LibreWolf")
               :program "librewolf")
    (chromium :jar chromium
              :directories ("~/.config/chromium"
                            ,@(when-let* ((xdg (getenv "XDG_CONFIG_HOME")))
                                (list (expand-file-name "chromium" xdg)))
                            "~/snap/chromium/common/chromium"
                            "~/.var/app/org.chromium.Chromium/config/chromium")
              :program browse-url-chromium-program
              :secret "Chromium Safe Storage")
    (chrome :jar chromium
            :directories ("~/.config/google-chrome"
                          ,@(when-let* ((xdg (getenv "XDG_CONFIG_HOME")))
                              (list (expand-file-name "google-chrome" xdg)))
                          "~/.var/app/com.google.Chrome/config/google-chrome")
            :program ("google-chrome-stable" "google-chrome" "chrome")
            :secret "Chrome Safe Storage")
    (brave :jar chromium
           :directories ("~/.config/BraveSoftware/Brave-Browser"
                         ,@(when-let* ((xdg (getenv "XDG_CONFIG_HOME")))
                             (list (expand-file-name "BraveSoftware/Brave-Browser" xdg)))
                         "~/.var/app/com.brave.Browser/config/BraveSoftware/Brave-Browser")
           :program ("brave" "brave-browser")
           :secret "Brave Safe Storage"))
  "The browsers a `(cookie :browser ...)' server can take its cookie from.
Each entry is (NAME . PLIST), NAME being what :browser names:
  :jar          `mozilla' (a cookies.sqlite in each profile) or
                `chromium' (a Cookies or Network/Cookies database, the
                profiles listed in the user data directory's Local State)
  :directories  where its profiles live; the first existing one gives
                the default profile, all are searched
  :program      what opens the sign-in page: a program, a list of
                candidates, or a variable holding one
  :secret       `chromium' only: the keyring item holding the key to its
                v11 cookies
Add an entry for a browser of either family that isn't here.  Chromium
on macOS keeps its key in the Keychain and derives it differently, so
that isn't read."
  :type '(alist :key-type symbol :value-type plist)
  :group 'scooter)

(defcustom scooter-auth-browser-profile nil
  "One browser profile directory to read cookies from.
nil means every profile of every browser the server's :browser names,
the default or most recently used one first."
  :type '(choice (const :tag "Every profile" nil) directory)
  :group 'scooter)

(defcustom scooter-auth-browser-check-path "/oauth2/auth"
  "Path on the server that answers 2xx only to a signed-in cookie.
The default is oauth2-proxy's own auth check."
  :type 'string
  :group 'scooter)

(defcustom scooter-auth-browser-poll-interval 1
  "Seconds between looks at the browser profiles during a sign-in."
  :type 'number
  :group 'scooter)

(defvar scooter-auth-quiet nil
  "Non-nil while auth may neither prompt nor open a browser.
Background work (change streams, timers) binds it: with no usable cookie
it gets `scooter-auth-needed' instead of a sign-in the user didn't ask for.")

(define-error 'scooter-auth-needed "Not signed in" 'user-error)

(defun scooter-auth--browsers (server)
  "Return the (NAME . PLIST) browsers SERVER's :browser names.
t names every one in `scooter-auth-browsers'."
  (let ((want (plist-get (cdr (scooter-auth--backend server)) :browser)))
    (cond ((eq want t) scooter-auth-browsers)
          ((assq want scooter-auth-browsers) (list (assq want scooter-auth-browsers)))
          (t (user-error "Scooter[%s]: unknown :browser %S; one of %s, or t for any"
                         server want
                         (mapconcat (lambda (b) (symbol-name (car b))) scooter-auth-browsers ", "))))))

(defun scooter-auth--browsers-label (browsers)
  "Return how to call BROWSERS to the user."
  (if (cdr browsers) "your browser" (capitalize (symbol-name (caar browsers)))))

(defun scooter-auth--browser-jar (browser profile)
  "Return the cookie database of BROWSER's PROFILE directory, or nil."
  (seq-find #'file-exists-p
            (mapcar (lambda (file) (expand-file-name file profile))
                    (pcase (plist-get (cdr browser) :jar)
                      ('mozilla '("cookies.sqlite"))
                      ('chromium '("Network/Cookies" "Cookies"))))))

(defun scooter-auth--browser-profiles (browser)
  "Return BROWSER's profile directories that hold a cookie jar, best first.
Just `scooter-auth-browser-profile' when that is set."
  (let ((dirs (if scooter-auth-browser-profile
                  (list scooter-auth-browser-profile)
                (let ((roots (plist-get (cdr browser) :directories)))
                  (pcase (plist-get (cdr browser) :jar)
                    ('mozilla (scooter-auth--mozilla-profiles roots))
                    ('chromium (scooter-auth--chromium-profiles roots)))))))
    (seq-filter (lambda (dir) (scooter-auth--browser-jar browser dir))
                (delete-dups (mapcar (lambda (dir) (directory-file-name (expand-file-name dir)))
                                     (delq nil dirs))))))

(defun scooter-auth--ini-sections (file)
  "Return FILE's INI sections as a list of (NAME . ALIST)."
  (with-temp-buffer
    (insert-file-contents file)
    (let (sections)
      (dolist (line (split-string (buffer-string) "\r?\n"))
        (setq line (string-trim line))
        (cond
         ((string-match "\\`\\[\\(.+\\)\\]\\'" line)
          (push (list (match-string 1 line)) sections))
         ((and sections (string-match "\\`\\([^=;#]+?\\)[ \t]*=[ \t]*\\(.*\\)\\'" line))
          (setcdr (car sections) (cons (cons (match-string 1 line) (match-string 2 line))
                                       (cdar sections))))))
      (nreverse sections))))

(defun scooter-auth--mozilla-default-profile (roots)
  "Return the profile the first of ROOTS' profiles.ini opens by default, or nil."
  (when-let* ((root (seq-find (lambda (dir) (file-exists-p (expand-file-name "profiles.ini" dir)))
                              roots))
              (sections (scooter-auth--ini-sections (expand-file-name "profiles.ini" root))))
    (let* ((get (lambda (section key) (cdr (assoc key (cdr section)))))
           (installed (seq-find (lambda (s) (and (string-prefix-p "Install" (car s))
                                                 (funcall get s "Default")))
                                sections))
           (profiles (seq-filter (lambda (s) (string-prefix-p "Profile" (car s))) sections))
           (profile (or (seq-find (lambda (s) (equal (funcall get s "Default") "1")) profiles)
                        (car profiles))))
      (cond
       (installed (expand-file-name (funcall get installed "Default") root))
       ((funcall get profile "Path")
        (if (equal (funcall get profile "IsRelative") "0")
            (funcall get profile "Path")
          (expand-file-name (funcall get profile "Path") root)))))))

(defun scooter-auth--mozilla-profiles (roots)
  "Return the profile directories under ROOTS, as Firefox lays them out.
The default profile first, then every other profile in each
profiles.ini, then any directory under those roots that holds a
cookies.sqlite."
  (let ((found (list (scooter-auth--mozilla-default-profile roots))))
    (dolist (root roots)
      (let ((ini (expand-file-name "profiles.ini" root)))
        (dolist (section (and (file-exists-p ini) (ignore-errors (scooter-auth--ini-sections ini))))
          (when-let* (((string-prefix-p "Profile" (car section)))
                      (path (cdr (assoc "Path" (cdr section)))))
            (push (if (equal (cdr (assoc "IsRelative" (cdr section))) "0")
                      path
                    (expand-file-name path root))
                  found))))
      ;; macOS keeps them in Profiles/.
      (dolist (dir (list root (expand-file-name "Profiles" root)))
        (dolist (db (and (file-directory-p dir)
                         (file-expand-wildcards (expand-file-name "*/cookies.sqlite" dir))))
          (push (file-name-directory db) found))))
    (nreverse (delq nil found))))

(defun scooter-auth--chromium-profiles (roots)
  "Return the profile directories under the Chromium user data ROOTS.
The last used one first, then the rest of each Local State's, most
recently active first, then any other directory there with a jar."
  (let (found)
    (dolist (root roots)
      (let ((state (expand-file-name "Local State" root)))
        (when-let* (((file-exists-p state))
                    (profile (ignore-errors
                               (alist-get 'profile (scooter-http-parse
                                                    (with-temp-buffer
                                                      (insert-file-contents state)
                                                      (buffer-string)))))))
          (let* ((cache (alist-get 'info_cache profile))
                 (active (lambda (entry) (or (alist-get 'active_time (cdr entry)) 0)))
                 (names (mapcar (lambda (entry) (symbol-name (car entry)))
                                (sort (copy-sequence cache)
                                      (lambda (a b) (> (funcall active a) (funcall active b)))))))
            (dolist (name (cons (alist-get 'last_used profile) names))
              (when name (setq found (append found (list (expand-file-name name root)))))))))
      (when (file-directory-p root)
        (dolist (db (append (file-expand-wildcards (expand-file-name "*/Network/Cookies" root))
                            (file-expand-wildcards (expand-file-name "*/Cookies" root))))
          (setq found (append found (list (string-remove-suffix
                                           "/Network" (directory-file-name (file-name-directory db)))))))))
    found))

(defun scooter-auth--cookie-hosts (host)
  "Return the jar hosts whose cookies a request to HOST carries.
Most specific first: HOST's own cookies, then domain cookies for HOST
and each parent domain."
  (let ((labels (split-string host "\\.")) hosts)
    (while labels
      (push (concat "." (string-join labels ".")) hosts)
      (setq labels (cdr labels)))
    (cons host (nreverse hosts))))

(defun scooter-auth--with-jar-copy (jar fn)
  "Call FN with an open sqlite handle on a copy of JAR.
The WAL, shared memory and rollback journal come along, so the
browser's lock and unflushed writes don't matter."
  (let* ((dir (make-temp-file "scooter-cookies" t))
         (copy (expand-file-name (file-name-nondirectory jar) dir)))
    (unwind-protect
        (progn
          (dolist (suffix '("" "-wal" "-shm" "-journal"))
            (when (file-exists-p (concat jar suffix))
              (copy-file (concat jar suffix) (concat copy suffix) t)))
          (let ((db (sqlite-open copy)))
            (unwind-protect (funcall fn db)
              (sqlite-close db))))
      (delete-directory dir t))))

(defun scooter-auth--join-cookies (rows hosts name)
  "Return the cookie values in ROWS, best first.
ROWS are (HOST ORIGIN NAME VALUE) in the jar's order of preference;
HOSTS orders them by host.  A session oauth2-proxy split into NAME_0,
NAME_1, ... comes back joined."
  (let (values)
    (dolist (h hosts)
      (dolist (origin (delete-dups (mapcar #'cadr (seq-filter (lambda (r) (equal (car r) h)) rows))))
        (let* ((jar (seq-filter (lambda (r) (and (equal (car r) h) (equal (cadr r) origin))) rows))
               (whole (seq-find (lambda (r) (equal (nth 2 r) name)) jar))
               (index (lambda (r) (string-to-number (substring (nth 2 r) (1+ (length name))))))
               (parts (sort (seq-remove (lambda (r) (eq r whole)) jar)
                            (lambda (a b) (< (funcall index a) (funcall index b))))))
          (push (if whole (nth 3 whole) (mapconcat (lambda (r) (nth 3 r)) parts "")) values))))
    (delete-dups (delete "" (nreverse values)))))

(defun scooter-auth--mozilla-cookies (jar host &optional name)
  "Return the cookie values named NAME that the cookies.sqlite JAR sends to HOST.
NAME defaults to `scooter-auth-default-cookie-name'.  Best candidates first."
  (let ((name (or name scooter-auth-default-cookie-name)))
    (when (and (sqlite-available-p) (file-exists-p jar))
      (let ((hosts (scooter-auth--cookie-hosts host)))
        (scooter-auth--join-cookies
         (scooter-auth--with-jar-copy
          jar
          (lambda (db)
            (sqlite-select db (format "SELECT host, originAttributes, name, value FROM moz_cookies
 WHERE host IN (%s) AND (name = ? OR name GLOB ?)
 ORDER BY originAttributes != '', lastAccessed DESC"
                                      (mapconcat (lambda (_) "?") hosts ","))
                           (append hosts (list name (concat name "_[0-9]*"))))))
         hosts name)))))

(defvar scooter-auth--undecryptable 0
  "How many Chromium cookies the last look at the jars couldn't decrypt.")

(defun scooter-auth--chromium-cookies (jar host &optional name secret)
  "Return the cookie values named NAME that the Chromium JAR sends to HOST.
NAME defaults to `scooter-auth-default-cookie-name'.  SECRET names the
keyring item holding the key to v11 values.  Best candidates first;
values that can't be decrypted are counted in `scooter-auth--undecryptable'."
  (let ((name (or name scooter-auth-default-cookie-name)))
    (when (and (sqlite-available-p) (file-exists-p jar))
      (let ((hosts (scooter-auth--cookie-hosts host)))
        (scooter-auth--with-jar-copy
         jar
         (lambda (db)
           (let ((version (string-to-number
                           (or (caar (sqlite-select db "SELECT value FROM meta WHERE key = 'version'"))
                               "0")))
                 (rows (sqlite-select db (format "SELECT host_key, name, value, encrypted_value FROM cookies
 WHERE host_key IN (%s) AND (name = ? OR name GLOB ?)
 ORDER BY last_access_utc DESC"
                                                 (mapconcat (lambda (_) "?") hosts ","))
                                      (append hosts (list name (concat name "_[0-9]*"))))))
             (scooter-auth--join-cookies
              (delq nil
                    (mapcar (lambda (row)
                              (pcase-let ((`(,h ,n ,value ,encrypted) row))
                                (let ((v (if (and (stringp value) (not (string-empty-p value)))
                                             value
                                           (scooter-auth--chromium-decrypt encrypted h version secret))))
                                  (if v
                                      (list h "" n v)
                                    (cl-incf scooter-auth--undecryptable)
                                    nil))))
                            rows))
              hosts name))))))))

(defun scooter-auth--chromium-key (password)
  "Return the AES key Chromium derives from its store PASSWORD.
PBKDF2-HMAC-SHA1 over the salt \"saltysalt\", one round, 16 bytes."
  (substring (gnutls-hash-mac 'SHA1 (copy-sequence password)
                              (concat "saltysalt" (unibyte-string 0 0 0 1)))
             0 16))

(defvar scooter-auth--safe-storage nil
  "Alist of keyring item label to the store password read from it.")

(declare-function secrets-list-collections "secrets")
(declare-function secrets-get-secret "secrets")

(defun scooter-auth--safe-storage-password (label)
  "Return the password in the Secret Service item called LABEL, or nil.
Chromium keeps the password behind its v11 cookies there, in an item
such as \"Chromium Safe Storage\"."
  (when label
    (or (cdr (assoc label scooter-auth--safe-storage))
        (when-let* ((password
                     (and (featurep 'dbusbind)
                          (require 'secrets nil t)
                          (bound-and-true-p secrets-enabled)
                          (seq-some (lambda (collection)
                                      (ignore-errors (secrets-get-secret collection label)))
                                    (ignore-errors (secrets-list-collections))))))
          (push (cons label password) scooter-auth--safe-storage)
          password))))

(defun scooter-auth--chromium-decrypt (blob host version secret)
  "Decrypt Chromium's encrypted cookie BLOB, set for HOST, or return nil.
AES-128-CBC: \"v10\" blobs use the fixed password \"peanuts\", \"v11\"
ones the password in the keyring item SECRET.  VERSION is the jar's
schema version: from 24 on, the plaintext starts with the SHA-256 of
HOST, which binds a cookie to its host."
  (when-let* (((and (stringp blob) (> (length blob) 3)
                    (memq 'ciphers (gnutls-available-p))))
              (key (pcase (substring blob 0 3)
                     ("v10" (scooter-auth--chromium-key "peanuts"))
                     ("v11" (when-let* ((password (scooter-auth--safe-storage-password secret)))
                              (scooter-auth--chromium-key password)))))
              (plain (car (ignore-errors
                            (gnutls-symmetric-decrypt 'AES-128-CBC key (make-string 16 ?\s)
                                                      (substring blob 3)))))
              (pad (and (> (length plain) 0) (aref plain (1- (length plain)))))
              ((and (<= 1 pad 16) (<= pad (length plain)))))
    (let ((text (substring plain 0 (- (length plain) pad))))
      (when (>= version 24)
        (setq text (and (>= (length text) 32)
                        (equal (substring text 0 32) (secure-hash 'sha256 host nil nil t))
                        (substring text 32))))
      (and text (decode-coding-string text 'utf-8)))))

(defun scooter-auth--browser-cookies (browser profile host name)
  "Return the cookie values named NAME that BROWSER's PROFILE sends to HOST."
  (when-let* ((jar (scooter-auth--browser-jar browser profile)))
    (pcase (plist-get (cdr browser) :jar)
      ('mozilla (scooter-auth--mozilla-cookies jar host name))
      ('chromium (scooter-auth--chromium-cookies jar host name (plist-get (cdr browser) :secret))))))

(defun scooter-auth--cookie-valid-p (server cookie)
  "Non-nil when SERVER's auth proxy accepts the session COOKIE."
  (pcase (ignore-errors
           (scooter-http-curl "GET" (scooter-server-url server scooter-auth-browser-check-path)
                              :headers `(("Cookie" . ,(concat (scooter-auth--cookie-name server)
                                                              "=" cookie)))))
    (`(,status ,ctype ,_) (and (<= 200 status 299) (not (scooter-http-auth-wall-p status ctype))))))

(defun scooter-auth--note-failure (server why)
  "Remember that SERVER's sign-in failed for WHY.
Until the user runs another command, requests for SERVER fail at once
instead of opening the sign-in page again: one wait per command."
  (scooter-auth--put server :sign-in-failed why)
  (add-hook 'pre-command-hook #'scooter-auth--forget-failures))

(defun scooter-auth--forget-failures ()
  "Let sign-ins try again: the user is running a new command."
  (remove-hook 'pre-command-hook #'scooter-auth--forget-failures)
  (maphash (lambda (name state)
             (when (plist-get state :sign-in-failed)
               (puthash name (plist-put state :sign-in-failed nil) scooter-auth--cache)))
           scooter-auth--cache))

(defun scooter-auth--browser-sign-in (server)
  "Return a signed-in cookie from SERVER's browsers, signing in there if needed.
Never asks to paste the cookie: when the browsers' cookies can't be read
it signals an error saying why."
  (unless (sqlite-available-p)
    (user-error "Scooter[%s]: this Emacs has no sqlite support, so it can't read a browser's cookies"
                server))
  (let* ((host (scooter-server-host server))
         (name (scooter-auth--cookie-name server))
         (browsers (scooter-auth--browsers server))
         (label (scooter-auth--browsers-label browsers))
         (rejected nil))
    (unless (seq-some #'scooter-auth--browser-profiles browsers)
      (user-error "Scooter[%s]: no %s profile with a cookie jar; set `scooter-auth-browser-profile'"
                  server (mapconcat (lambda (b) (capitalize (symbol-name (car b)))) browsers "/")))
    (let* ((look (lambda ()
                   (setq scooter-auth--undecryptable 0)
                   (seq-find (lambda (value)
                               (or (scooter-auth--cookie-valid-p server value)
                                   (progn (push value rejected) nil)))
                             (seq-difference
                              (delete-dups
                               ;; Listed afresh each time: signing in may land in
                               ;; a profile that didn't exist a moment ago.
                               (mapcan (lambda (browser)
                                         (mapcan (lambda (profile)
                                                   (ignore-errors
                                                     (scooter-auth--browser-cookies browser profile host name)))
                                                 (scooter-auth--browser-profiles browser)))
                                       browsers))
                              rejected))))
           (found (funcall look)))
      (unless found
        (condition-case nil
            (with-timeout (scooter-auth-login-timeout
                           (let ((why (format "sign-in not finished after %d seconds%s"
                                              scooter-auth-login-timeout
                                              (if (> scooter-auth--undecryptable 0)
                                                  (format " (%d Chromium cookies couldn't be decrypted: the keyring secret isn't readable)"
                                                          scooter-auth--undecryptable)
                                                ""))))
                             (scooter-auth--note-failure server why)
                             (user-error "Scooter[%s]: %s" server why)))
              (let ((rd (url-filename (url-generic-parse-url (scooter-server-url server)))))
                (scooter-auth--browse
                 browsers
                 (scooter-server-url server (concat "/oauth2/start?rd="
                                                    (url-hexify-string (if (string-empty-p rd) "/" rd))))))
              (message "Scooter[%s]: sign in in %s (C-g cancels)..." server label)
              (while (not (setq found (funcall look)))
                (accept-process-output nil scooter-auth-browser-poll-interval)))
          (quit (scooter-auth--note-failure server "sign-in cancelled")
                (signal 'quit nil))))
      (message "Scooter[%s]: signed in" server)
      (scooter-auth-set-cookie server found))))

(defun scooter-auth--browser-program (browser)
  "Return the program (or list of candidates) that opens BROWSER."
  (let ((program (plist-get (cdr browser) :program)))
    (if (and (symbolp program) (boundp program)) (symbol-value program) program)))

(defun scooter-auth--browse (browsers url)
  "Open URL in the browser the sign-in reads cookies from.
For one browser in BROWSERS, that is its program; for several, or when
the program isn't around, whatever `browse-url' picks."
  (let ((program (and (null (cdr browsers))
                      (seq-find #'executable-find
                                (ensure-list (scooter-auth--browser-program (car browsers)))))))
    (if program
        (let ((browse-url-generic-program program)
              (browse-url-generic-args nil))
          (browse-url-generic url))
      (browse-url url))))

;;;###autoload
(defun scooter-auth-browser-login (server)
  "Sign in to SERVER through its browser and save the session cookie.
Reuses the browser's session when it has a live one; otherwise opens
the sign-in page there and waits for it to finish."
  (interactive (list (scooter-current-server t)))
  (scooter-auth--put server :sign-in-failed nil)
  (scooter-auth--signing-in server #'scooter-auth--browser-sign-in))

;;;; auth-source

(defun scooter-auth--secret (host user)
  "Return the auth-source secret for HOST and USER, or nil."
  (when-let* ((found (car (auth-source-search :host host :user user :max 1)))
              (secret (plist-get found :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun scooter-auth--save-secret (host user secret)
  "Save SECRET for HOST and USER through auth-source, replacing the old one."
  (ignore-errors
    (auth-source-forget (list :host host :user user))
    (unless (equal (scooter-auth--secret host user) secret)
      ;; `:create' only adds an entry when none matches.
      (scooter-auth--delete-secret host user)
      (let ((found (car (auth-source-search :host host :user user :port "https"
                                            :secret secret :create t :max 1))))
        (when-let* ((save (plist-get found :save-function)))
          (funcall save))))))

(defun scooter-auth--delete-secret (host user)
  "Delete HOST and USER's entries from every auth-source backend."
  (dolist (backend (mapcar #'auth-source-backend-parse auth-sources))
    (let ((source (oref backend source)))
      (ignore-errors
        (pcase (oref backend type)
          ('netrc (scooter-auth--netrc-delete source host user))
          ('secrets (dolist (item (secrets-search-items source :host host :user user))
                      (secrets-delete-item source item)))
          ('plstore (auth-source-search :backend backend :host host :user user
                                        :delete t :max 100))))))
  (auth-source-forget-all-cached))

(defun scooter-auth--netrc-delete (file host user)
  "Delete the one-line netrc entries for HOST and USER from FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((machine (rx (or bol blank) "machine" (+ blank) (literal host) (or blank eol)))
            (login (rx (or bol blank) (or "login" "user") (+ blank) (literal user) (or blank eol)))
            deleted)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring (pos-bol) (pos-eol))))
            (if (and (string-match-p machine line) (string-match-p login line))
                (progn (delete-region (pos-bol) (min (point-max) (1+ (pos-eol))))
                       (setq deleted t))
              (forward-line 1))))
        (when deleted
          (write-region (point-min) (point-max) file nil 'silent)
          (set-file-modes file #o600))))))

;;;; OIDC

(defun scooter-auth--jwt-exp (token)
  "Return the exp claim of JWT TOKEN, or 0."
  (or (ignore-errors
        (let* ((payload (nth 1 (split-string token "\\.")))
               (pad (make-string (% (- 4 (% (length payload) 4)) 4) ?=))
               (json (base64-decode-string (concat payload pad) t)))
          (alist-get 'exp (scooter-http-parse (decode-coding-string json 'utf-8)))))
      0))

(defun scooter-auth--discovery (issuer)
  "Return ISSUER's OpenID discovery document."
  (pcase-let ((`(,status ,_ ,text)
               (scooter-http-curl "GET" (concat (string-trim-right issuer "/")
                                                "/.well-known/openid-configuration"))))
    (unless (= status 200)
      (user-error "OIDC discovery for %s failed: %d" issuer status))
    (scooter-http-parse text)))

(defun scooter-auth--token-request (endpoint form)
  "POST FORM (an alist) to the token ENDPOINT and return the reply."
  (pcase-let ((`(,status ,_ ,text)
               (scooter-http-curl "POST" endpoint
                                  :body (url-build-query-string
                                         (mapcar (lambda (kv) (list (car kv) (cdr kv))) form))
                                  :content-type "application/x-www-form-urlencoded")))
    (let ((reply (scooter-http-parse text)))
      (unless (= status 200)
        (error "Token request failed: %d %s" status (or (alist-get 'error reply) text)))
      reply)))

(defun scooter-auth--id-token (server opts)
  "Return a live ID token for SERVER configured by OPTS."
  (let ((token (scooter-auth--get server :id-token)))
    (if (and token (> (scooter-auth--jwt-exp token) (+ (float-time) 60)))
        token
      (or (ignore-errors (scooter-auth--refresh server opts))
          (scooter-auth--signing-in server #'scooter-auth-login)))))

(defun scooter-auth--store (server opts reply)
  "Cache the tokens in REPLY for SERVER, persisting the refresh token per OPTS."
  (let ((id (alist-get 'id_token reply))
        (refresh (alist-get 'refresh_token reply)))
    (unless id (error "Scooter[%s]: the IdP returned no id_token" server))
    (scooter-auth--put server :id-token id)
    (when refresh
      (scooter-auth--put server :refresh-token refresh)
      (scooter-auth--save-secret (url-host (url-generic-parse-url (plist-get opts :issuer)))
                                 (plist-get opts :client-id) refresh))
    id))

(defun scooter-auth--refresh (server opts)
  "Refresh SERVER's ID token per OPTS, or return nil."
  (when-let* ((refresh (or (scooter-auth--get server :refresh-token)
                           (scooter-auth--secret
                            (url-host (url-generic-parse-url (plist-get opts :issuer)))
                            (plist-get opts :client-id)))))
    (let ((doc (scooter-auth--discovery (plist-get opts :issuer))))
      (scooter-auth--store
       server opts
       (scooter-auth--token-request
        (alist-get 'token_endpoint doc)
        `(("grant_type" . "refresh_token") ("refresh_token" . ,refresh)
          ("client_id" . ,(plist-get opts :client-id))))))))

(defun scooter-auth--random (n)
  "Return N random bytes, base64url-encoded."
  (base64url-encode-string
   (if (and (file-readable-p "/dev/urandom") (executable-find "head"))
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (let ((coding-system-for-read 'binary))
           (call-process "head" "/dev/urandom" t nil "-c" (number-to-string n)))
         (buffer-string))
     ;; No OS entropy file: fall back to the seeded PRNG.
     (random t)
     (apply #'unibyte-string (cl-loop repeat n collect (random 256))))
   t))

(defun scooter-auth-pkce-challenge (verifier)
  "Return the S256 PKCE challenge for VERIFIER."
  (base64url-encode-string (secure-hash 'sha256 verifier nil nil t) t))

;;;###autoload
(defun scooter-auth-login (server)
  "Log in to SERVER's IdP in a browser; return the ID token."
  (interactive (list (scooter-current-server t)))
  (let* ((opts (cdr (scooter-auth--backend server)))
         (doc (scooter-auth--discovery (plist-get opts :issuer)))
         (verifier (scooter-auth--random 48))
         (state (scooter-auth--random 16))
         (result nil)
         (proc (make-network-process
                :name "scooter-oidc-callback" :server t :host "127.0.0.1" :service t
                :family 'ipv4 :noquery t
                :filter (lambda (conn text)
                          (when (string-match "\\`GET \\([^ ]+\\)" text)
                            (let* ((query (url-parse-query-string
                                           (or (cadr (split-string (match-string 1 text) "?")) "")))
                                   (get (lambda (k) (cadr (assoc k query)))))
                              (setq result (if (equal (funcall get "state") state)
                                               (or (funcall get "code")
                                                   (list 'error (funcall get "error")))
                                             (list 'error "state mismatch")))
                              (process-send-string
                               conn (concat "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\n"
                                            "Scooter: you can close this tab.\n"))
                              (delete-process conn))))))
         (redirect (format "http://127.0.0.1:%d/callback" (process-contact proc :service))))
    (unwind-protect
        (progn
          (browse-url
           (concat (alist-get 'authorization_endpoint doc) "?"
                   (url-build-query-string
                    `(("response_type" "code")
                      ("client_id" ,(plist-get opts :client-id))
                      ("redirect_uri" ,redirect)
                      ("scope" ,(string-join (or (plist-get opts :scopes)
                                                 '("openid" "email" "profile" "offline_access"))
                                             " "))
                      ("state" ,state)
                      ("code_challenge" ,(scooter-auth-pkce-challenge verifier))
                      ("code_challenge_method" "S256")))))
          (message "Scooter[%s]: finish the login in your browser..." server)
          (with-timeout (scooter-auth-login-timeout
                         (user-error "Scooter[%s]: login timed out" server))
            (while (not result) (accept-process-output nil 0.2)))
          (when (consp result)
            (user-error "Scooter[%s]: login failed: %s" server (cadr result)))
          (prog1 (scooter-auth--store
                  server opts
                  (scooter-auth--token-request
                   (alist-get 'token_endpoint doc)
                   `(("grant_type" . "authorization_code") ("code" . ,result)
                     ("redirect_uri" . ,redirect) ("code_verifier" . ,verifier)
                     ("client_id" . ,(plist-get opts :client-id)))))
            (message "Scooter[%s]: logged in" server)))
      (delete-process proc))))

(provide 'scooter-auth)
;;; scooter-auth.el ends here
