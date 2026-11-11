;;; scooter-test.el --- Tests for scooter-mode  -*- lexical-binding: t; -*-

(require 'ert)
(require 'scooter)

;; Keep tests away from the user's auth-source files.
(setq auth-sources nil)

(ert-deftest scooter-test-slug ()
  (should (equal (scooter-slug "Fix the login redirect!" "abc") "fix-the-login-redirect"))
  (should (equal (scooter-slug "  --Ünïcode & spaces--  " "abc") "n-code-spaces"))
  (should (equal (scooter-slug nil "0123456789abcdef") "01234567"))
  (should (equal (scooter-slug "" "abc") "abc"))
  (should (<= (length (scooter-slug (make-string 60 ?a) "x")) 40)))

(ert-deftest scooter-test-status ()
  (should (eq (scooter-list-status '((status . "running") (awaitingInputAt . 5) (runStartedAt . 1))) 'awaiting))
  (should (eq (scooter-list-status '((status . "running") (runStartedAt . 1))) 'running))
  (should (eq (scooter-list-status '((status . "running"))) 'idle))
  (should (eq (scooter-list-status '((status . "suspended") (runStartedAt . 1))) 'suspended))
  (should (eq (scooter-list-status '((status . "failed") (awaitingInputAt . 5))) 'failed)))

(ert-deftest scooter-test-auth-wall ()
  (should (scooter-http-auth-wall-p 401 "application/json"))
  (should (scooter-http-auth-wall-p 302 ""))
  (should (scooter-http-auth-wall-p 403 "text/html"))
  (should (scooter-http-auth-wall-p 200 "text/html; charset=utf-8"))
  (should-not (scooter-http-auth-wall-p 403 "application/json"))
  (should-not (scooter-http-auth-wall-p 404 "application/json"))
  (should-not (scooter-http-auth-wall-p 200 "application/json")))

(ert-deftest scooter-test-jwt-exp ()
  (let ((token (concat "e30." (base64url-encode-string "{\"exp\":1700000000}" t) ".sig")))
    (should (= (scooter-auth--jwt-exp token) 1700000000)))
  (should (= (scooter-auth--jwt-exp "garbage") 0)))

(ert-deftest scooter-test-pkce ()
  ;; Expected value computed independently with Perl's Digest::SHA.
  (should (equal (scooter-auth-pkce-challenge "dBjftJeZ4CVP-mJ92K1hgPLBEsl4XBO6pqR1Vie9rF8")
                 "niQw9d7JkLeYdv9DhxacziU9zeJo1l1b1rGVtP0T2i4"))
  (let ((a (scooter-auth--random 32)) (b (scooter-auth--random 32)))
    (should (= (length a) 43))
    (should-not (equal a b))))

(ert-deftest scooter-test-cookie-replaces-saved-one ()
  (let* ((file (make-temp-file "scooter-authinfo"))
         (auth-sources (list file))
         (auth-source-save-behavior t)
         (scooter-servers '((home :url "https://s.example" :auth cookie)))
         (scooter-auth--cache (make-hash-table :test #'eq)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "machine other.example login me password keep\n"
                    "machine s.example login _oauth2_proxy password stale\n"))
          (scooter-auth-set-cookie 'home "fresh")
          ;; A restart: only auth-source remembers the cookie.
          (clrhash scooter-auth--cache)
          (auth-source-forget-all-cached)
          (should (equal (scooter-auth--cookie 'home) "fresh"))
          (with-temp-buffer
            (insert-file-contents file)
            (should (= (count-matches "_oauth2_proxy" (point-min) (point-max)) 1))
            (should (search-forward "password keep" nil t))))
      (delete-file file))))

(ert-deftest scooter-test-headers-json ()
  (let ((scooter-servers '((home :url "https://s.example" :auth cookie)
                           (dev :url "http://localhost:8080")))
        (scooter-auth--cache (make-hash-table :test #'eq)))
    (scooter-auth-set-cookie 'home "_oauth2_proxy=abc")
    (should (equal (json-parse-string (scooter-auth-headers-json "home") :object-type 'alist)
                   '((Cookie . "_oauth2_proxy=abc"))))
    (should (equal (scooter-auth-headers-json "dev") "{}"))))

(ert-deftest scooter-test-cookie-name-names-the-header-and-the-secret ()
  ;; A server whose oauth2-proxy runs with --cookie-name: the header, the
  ;; pasted prefix and the auth-source entry all use that name.
  (let ((scooter-servers '((home :url "https://s.example"
                                 :auth (cookie :cookie-name "__Host-scooter_oauth2"))))
        (scooter-auth--cache (make-hash-table :test #'eq))
        saved)
    (cl-letf (((symbol-function 'scooter-auth--save-secret)
               (lambda (_host user secret) (push (cons user secret) saved)))
              ((symbol-function 'scooter-auth--secret)
               (lambda (_host user) (and (equal user "__Host-scooter_oauth2") "stored"))))
      (scooter-auth-set-cookie 'home "__Host-scooter_oauth2=abc")
      (should (equal (scooter-auth-headers 'home) '(("Cookie" . "__Host-scooter_oauth2=abc"))))
      (should (equal saved '(("__Host-scooter_oauth2" . "abc"))))
      ;; A restart: the cookie comes back from auth-source under that name.
      (clrhash scooter-auth--cache)
      (should (equal (scooter-auth-headers 'home) '(("Cookie" . "__Host-scooter_oauth2=stored")))))))

(ert-deftest scooter-test-headers-command-keeps-placeholder ()
  (let ((server-socket-dir "/tmp/emacs-sock") (server-name "server"))
    (should (string-match-p "{retry})'\\'" (scooter-agent-shell-headers-command 'home)))))

(ert-deftest scooter-test-acp-arguments ()
  (let ((scooter-servers '((home :url "https://s.example/" :model "m1")))
        (scooter-acp-command '("scooter" "acp")))
    (cl-letf (((symbol-function 'scooter-agent-shell--headers-args) (lambda (_) '("--headers-file" "f"))))
      (should (equal (scooter-acp-arguments 'home :project t)
                     '("acp" "--server" "https://s.example" "--model" "m1" "--project" "--headers-file" "f"))))))

(ert-deftest scooter-test-acp-arguments-self ()
  (let ((scooter-servers '((sandbox :self t)))
        (scooter-acp-command '("scooter" "acp")))
    (should (equal (scooter-acp-arguments 'sandbox) '("acp" "--self")))))

(ert-deftest scooter-test-acp-arguments-replay-turns ()
  (let ((scooter-servers '((sandbox :self t)))
        (scooter-acp-command '("scooter" "acp"))
        (scooter-acp-replay-turns 30))
    (should (equal (scooter-acp-arguments 'sandbox) '("acp" "--self" "--replay-turns" "30")))))

(defconst scooter-test--dir (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory, captured at load time.")

(defmacro scooter-test-with-fake-server (url-var &rest body)
  "Run BODY with URL-VAR bound to a fake Scooter's base URL."
  (declare (indent 1))
  `(let* ((proc (start-process "fake-scooter" (generate-new-buffer " *fake*") "emacs" "-Q" "--batch" "-l"
                               (expand-file-name "fake-server.el" scooter-test--dir)))
          (port nil))
     (unwind-protect
         (progn
           (with-current-buffer (process-buffer proc)
             (with-timeout (10 (error "Fake server didn't start"))
               (while (not (string-match "^\\([0-9]+\\)$" (buffer-string)))
                 (accept-process-output proc 0.1)))
             (setq port (match-string 1 (buffer-string))))
           (let ((,url-var (format "http://127.0.0.1:%s" port)))
             ,@body))
       (delete-process proc))))

(ert-deftest scooter-test-request-retries-after-auth-wall ()
  (scooter-test-with-fake-server url
    (let ((scooter-servers `((home :url ,url :auth cookie)))
          (scooter-auth--cache (make-hash-table :test #'eq))
          (asked 0))
      (scooter-auth-set-cookie 'home "stale")
      (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
                ((symbol-function 'read-passwd) (lambda (&rest _) (cl-incf asked) "good"))
                ((symbol-function 'y-or-n-p) #'ignore))
        (should (equal (scooter-request 'home "GET" "/whoami") '((id . "me"))))
        (should (= asked 1))
        ;; The fresh cookie is reused.
        (should (equal (scooter-request 'home "GET" "/whoami") '((id . "me"))))
        (should (= asked 1))))))

(ert-deftest scooter-test-request-gives-up-after-one-retry ()
  (scooter-test-with-fake-server url
    (let ((scooter-servers `((home :url ,url :auth cookie)))
          (scooter-auth--cache (make-hash-table :test #'eq)))
      (scooter-auth-set-cookie 'home "stale")
      (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
                ((symbol-function 'read-passwd) (lambda (&rest _) "still-bad"))
                ((symbol-function 'y-or-n-p) #'ignore))
        (should-error (scooter-request 'home "GET" "/whoami") :type 'scooter-auth-wall)))))

(ert-deftest scooter-test-nested-sign-in-does-not-prompt-again ()
  ;; While one shell's cookie prompt is open, other shells' bridges run
  ;; emacsclient too; their evals run nested inside the open prompt.  They
  ;; get "null" (ask again shortly), not a second prompt or an error, and
  ;; the next ask after the prompt closes gets the new cookie.
  (let ((scooter-servers '((home :url "https://s.example" :auth cookie)))
        (scooter-auth--cache (make-hash-table :test #'eq))
        (asked 0)
        nested)
    (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
              ((symbol-function 'scooter-auth--save-secret) #'ignore)
              ((symbol-function 'y-or-n-p) #'ignore)
              ((symbol-function 'read-passwd)
               (lambda (&rest _)
                 (cl-incf asked)
                 (when (= asked 1)
                   (setq nested (list (scooter-auth-headers-json "home" t)
                                      (scooter-auth-headers-json "home" t)
                                      (scooter-auth-headers-json "home"))))
                 "fresh")))
      (scooter-auth-set-cookie 'home "stale")
      (should (equal (scooter-auth-headers 'home t) '(("Cookie" . "_oauth2_proxy=fresh"))))
      (should (= asked 1))
      (should (equal nested '("null" "null" "null")))
      (should (equal (scooter-auth-headers-json "home") "{\"Cookie\":\"_oauth2_proxy=fresh\"}"))
      (should (= asked 1)))))

(ert-deftest scooter-test-nested-elisp-request-during-sign-in-errors ()
  ;; Emacs's own requests (a list refresh timer) nested in the prompt get a
  ;; user-error, not a second prompt.
  (let ((scooter-servers '((home :url "https://s.example" :auth cookie)))
        (scooter-auth--cache (make-hash-table :test #'eq))
        (asked 0)
        nested)
    (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
              ((symbol-function 'scooter-auth--save-secret) #'ignore)
              ((symbol-function 'y-or-n-p) #'ignore)
              ((symbol-function 'read-passwd)
               (lambda (&rest _)
                 (cl-incf asked)
                 (setq nested (condition-case err (scooter-auth-headers 'home)
                                (user-error (error-message-string err))))
                 "fresh")))
      (scooter-auth-headers 'home t)
      (should (= asked 1))
      (should (string-match-p "already signing in" nested)))))

(ert-deftest scooter-test-json-403-is-an-error-not-a-wall ()
  (scooter-test-with-fake-server url
    (let ((scooter-servers `((home :url ,url))))
      (let ((err (should-error (scooter-request 'home "POST" "/forbidden") :type 'scooter-http-error)))
        (should (string-match-p "not your conversation" (format "%s" err)))))))




(ert-deftest scooter-test-agent-shell-config-runs-the-bridge ()
  (let ((scooter-servers '((home :url "https://s.example")))
        (scooter-acp-command '("scooter" "acp")))
    (cl-letf (((symbol-function 'scooter-agent-shell--headers-args) (lambda (_) '("--headers-file" "f")))
              ((symbol-function 'scooter-install-program) #'identity))
      (let* ((config (scooter-agent-shell-config 'home :title "fix-login"))
             (client (with-temp-buffer (funcall (map-elt config :client-maker) (current-buffer)))))
        (should (equal (map-elt config :buffer-name) "Scooter fix-login@home"))
        (should (member "--server" (map-elt client :command-params)))
        (should (equal (cons (map-elt client :command) (map-elt client :command-params))
                       '("scooter" "acp" "--server" "https://s.example" "--headers-file" "f")))))))

(ert-deftest scooter-test-agent-shell-forms-are-buffer-local ()
  (let ((scooter-servers '((home :url "https://s.example"))))
    (cl-letf (((symbol-function 'scooter-agent-shell--headers-args) (lambda (_) nil))
              ((symbol-function 'scooter-install-program) #'identity))
      (dolist (on '(t nil))
        (let ((scooter-agent-shell-forms on)
              (config (scooter-agent-shell-config 'home)))
          (with-temp-buffer
            (funcall (map-elt config :client-maker) (current-buffer))
            (should (eq agent-shell-elicitation--experimental-feature-enabled on)))))
      (should-not (default-value 'agent-shell-elicitation--experimental-feature-enabled)))))

(ert-deftest scooter-test-install-verifies-and-writes ()
  (let* ((bin (unibyte-string 0 127 128 255 10 13))
         (asset (concat "scooter-" (scooter-install--platform)))
         (user-emacs-directory (make-temp-file "scooter-ued" t))
         (sums (format "%s  %s\n" (secure-hash 'sha256 bin) asset)))
    (cl-letf (((symbol-function 'scooter-install--fetch)
               (lambda (url) (if (string-suffix-p "SHA256SUMS" url) sums bin))))
      (let ((path (scooter-install "9.9.9")))
        (should (file-executable-p path))
        (should (equal (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally path)
                         (buffer-string))
                       bin))))))

(ert-deftest scooter-test-install-rejects-bad-checksum ()
  (let ((user-emacs-directory (make-temp-file "scooter-ued" t))
        (asset (concat "scooter-" (scooter-install--platform))))
    (cl-letf (((symbol-function 'scooter-install--fetch)
               (lambda (url) (if (string-suffix-p "SHA256SUMS" url)
                                 (format "%s  %s\n" (make-string 64 ?0) asset)
                               "tampered"))))
      (should-error (scooter-install "9.9.9"))
      (should-not (file-exists-p (scooter-install-path "9.9.9"))))))

(ert-deftest scooter-test-install-program-prefers-path ()
  (let ((scooter-install-binary nil))
    (should (equal (scooter-install-program "sh") (executable-find "sh")))
    (should-error (scooter-install-program "no-such-scooter-binary") :type 'user-error)))

(defun scooter-test--lisp-dir ()
  "The package's lisp/ directory."
  (file-name-directory (locate-library "scooter")))

(ert-deftest scooter-test-list-from-autoloads-in-fresh-emacs ()
  ;; One process with everything loaded hides missing requires; start cold.
  (scooter-test-with-fake-server url
    (let* ((dir (make-temp-file "scooter-autoloads" t))
           (defs (expand-file-name "scooter-autoloads.el" dir))
           (form `(progn
                    (setq load-path ',load-path)
                    (load ,defs nil t)
                    (setq scooter-servers '((home :url ,url)))
                    (scooter-list 'home)
                    (princ (with-current-buffer "*scooter: home*" (buffer-string))))))
      (loaddefs-generate (scooter-test--lisp-dir) defs)
      (with-temp-buffer
        (let ((rc (call-process "emacs" nil t nil "-Q" "--batch" "--eval" (prin1-to-string form))))
          (should (= rc 0))
          (should (string-match-p "^★ Starred (1)\n● Fix login +m1 .* [0-9]+d$" (buffer-string)))
          (should (string-match-p "^Recent (1)\n○ 01234567 " (buffer-string)))
          (should-not (string-match-p "void" (buffer-string))))))))

(defmacro scooter-test-with-list (convs &rest body)
  "Run BODY in a list buffer showing CONVS on server home."
  (declare (indent 1))
  `(let ((scooter-servers '((home :url "https://s.example"))))
     (cl-letf (((symbol-function 'scooter-list--fetch) (lambda (_) ,convs))
               ((symbol-function 'scooter-list--start-streams) #'ignore))
       (save-window-excursion
         (scooter-list 'home)
         (unwind-protect (progn ,@body)
           (kill-buffer "*scooter: home*"))))))

(ert-deftest scooter-test-list-rows-align ()
  (let ((now (* 1000 (float-time))))
    (scooter-test-with-list
        (list `((id . "a") (title . "Short") (starred . t) (runStartedAt . 1) (lastActivityAt . ,(- now 540000)))
              `((id . "b") (title . "A much longer conversation title") (model . "m2") (runStartedAt . 1)
                (lastActivityAt . ,(- now 7200000))))
      (should (string-match-p "^● Short +.* 9m$" (buffer-string)))
      (let ((cols (mapcar (lambda (re) (goto-char (point-min)) (re-search-forward re) (current-column))
                          '(" 9m" " 2h"))))
        (should (apply #'= cols))))))

(ert-deftest scooter-test-list-header-matches-columns ()
  (scooter-test-with-list (list '((id . "a") (title . "T") (model . "m1") (owner . "me@x")))
    (should (string-match-p "\\`home  https://s.example  1 conversation\n +Title +Model +Links +Owner +Active\n"
                            (buffer-string)))
    (cl-flet ((col (re) (goto-char (point-min)) (re-search-forward re) (goto-char (match-beginning 0)) (current-column)))
      (should (= (col "Model") (col "m1"))))))

(ert-deftest scooter-test-list-link-labels ()
  (should (equal (scooter-list--col-links
                  '((links . [((source . "github") (url . "https://github.com/o/r/pull/12"))
                              ((source . "slack"))])))
                 "#12 slack"))
  (should (equal (scooter-list--col-links '((sources "jira"))) "jira")))

(ert-deftest scooter-test-list-details-fold-and-persist ()
  (scooter-test-with-list (list '((id . "conv-1") (title . "Topic") (owner . "me@x")))
    (goto-char (point-min))
    (search-forward "Topic")
    (should (oref (magit-current-section) hidden))
    (magit-section-show (magit-current-section))
    (should (string-match-p "^ +id +conv-1$" (buffer-string)))
    (should (string-match-p "^ +url +https://s.example/chat/conv-1$" (buffer-string)))
    (scooter-list-refresh)
    (should (string-match-p "^ +id +conv-1$" (buffer-string)))))

(ert-deftest scooter-test-list-status-icons ()
  (scooter-test-with-list
      (list '((id . "a") (title . "Wait") (awaitingInputAt . 1) (model . "m1"))
            '((id . "b") (title . "Run") (runStartedAt . 1))
            '((id . "c") (title . "Boom") (status . "failed") (starred . t)))
    (cl-flet ((icon (title) (goto-char (point-min)) (search-forward title) (char-after (pos-bol))))
      (should (equal (string (icon "Wait")) "!"))
      (should (eq (get-text-property (pos-bol) 'font-lock-face) 'scooter-list-awaiting))
      (should (string-prefix-p "Waiting on you for " (get-text-property (pos-bol) 'help-echo)))
      (icon "Run")
      (should (eq (get-text-property (pos-bol) 'font-lock-face) 'scooter-list-running))
      (should (string-match-p "\\`Agent working.*\nA turn is in progress\\.\\'"
                              (get-text-property (pos-bol) 'help-echo)))
      (icon "m1")
      (should (eq (get-text-property (1- (point)) 'font-lock-face) 'scooter-list-model)))))

(ert-deftest scooter-test-list-starred-first ()
  (scooter-test-with-list
      (list '((id . "a") (title . "Newest"))
            '((id . "b") (title . "Older") (starred . t)))
    (cl-flet ((pos (text) (string-search text (buffer-string))))
      (should (< (pos "★ Starred (1)\n● Older") (pos "Recent (1)\n● Newest"))))))

(ert-deftest scooter-test-list-shows-fetch-errors ()
  (scooter-test-with-list "Scooter[home]: login required"
    (should (string-match-p "^  Scooter\\[home\\]: login required$" (buffer-string)))))

(ert-deftest scooter-test-list-help-key ()
  (with-temp-buffer
    (scooter-list-mode)
    (should (eq (key-binding "?") #'scooter-list-dispatch))))

(defun scooter-test--fresh-emacs (&rest forms)
  "Run FORMS in a fresh Emacs with this `load-path'; return (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((rc (call-process (expand-file-name invocation-name invocation-directory)
                            nil t nil "-Q" "--batch" "--eval"
                            (prin1-to-string `(progn (setq load-path ',load-path) ,@forms)))))
      (cons rc (buffer-string)))))

(defmacro scooter-test--skip-unless-installed (feature)
  "Skip unless FEATURE is installed; Cask installs it from MELPA."
  `(skip-unless (locate-library ,(symbol-name feature))))

(ert-deftest scooter-test-projectile-binds-s ()
  ;; Real MELPA Projectile, loaded before and after scooter-project.
  (scooter-test--skip-unless-installed projectile)
  (dolist (order '((projectile scooter-project) (scooter-project projectile)))
    (let ((res (scooter-test--fresh-emacs
                `(mapc #'require ',order)
                '(princ (keymap-lookup projectile-command-map "S")))))
      (should (equal res '(0 . "scooter-new-from-project"))))))

(ert-deftest scooter-test-list-under-real-evil ()
  ;; Real MELPA evil: the list opens in Emacs state so its keys work.
  (scooter-test--skip-unless-installed evil)
  (scooter-test-with-fake-server url
    (let ((res (scooter-test--fresh-emacs
                '(require 'evil) '(evil-mode 1) '(require 'scooter-list)
                `(setq scooter-servers '((home :url ,url)))
                '(scooter-list 'home)
                '(princ (list evil-state (key-binding "?") (key-binding "g"))))))
      (should (equal res '(0 . "(emacs scooter-list-dispatch scooter-list-refresh)"))))))

;;; Tramp

(require 'scooter-tramp)

(ert-deftest scooter-test-tramp-file-name-prefers-a-unique-slug ()
  (cl-letf (((symbol-function 'scooter--title-of) (lambda (_s id) (if (equal id "old") "dup" "fix")))
            ((symbol-function 'scooter-tramp-resolve) (lambda (_s slug) (if (equal slug "dup") "new" "c1"))))
    (should (equal (scooter-tramp-file-name 'home "c1") "/scooter:fix@home:/workspace/"))
    (should (equal (scooter-tramp-file-name 'home "old" "/tmp/") "/scooter:old@home:/tmp/"))))

(ert-deftest scooter-test-tramp-resolves-slugs-and-id-prefixes ()
  (scooter-test-with-fake-server url
    (let ((scooter-servers `((home :url ,url))))
      (should (equal (scooter-tramp-resolve 'home "fix-login") "c1"))
      (should (equal (scooter-tramp-resolve 'home "0123") "0123456789"))
      (should-error (scooter-tramp-resolve 'home "nope") :type 'user-error)
      (should (equal (scooter-tramp-completions "") '(("fix-login" "home") ("01234567" "home")))))))

(ert-deftest scooter-test-tramp-writes-a-private-spec ()
  (scooter-test-with-fake-server url
    (let* ((scooter-servers `((home :url ,url)))
           (scooter-tramp-directory (file-name-as-directory (make-temp-file "scooter-tramp" t)))
           (file (scooter-tramp--file "fix-login.home.json")))
      (unwind-protect
          (progn
            (scooter-tramp--write-spec "home" "fix-login")
            (should (= (file-modes file) #o600))
            (should (equal (json-read-file file)
                           `((url . ,url) (conversation . "c1") (headers)))))
        (delete-directory scooter-tramp-directory t)))))

(ert-deftest scooter-test-tramp-login-args-survive-the-shell ()
  ;; Tramp joins the args with spaces and hands them to a shell.
  (cl-letf (((symbol-function 'scooter-install-program) #'identity))
    (let* ((scooter-tramp-directory "/tmp/a dir/")
           (scooter-acp-command '("/opt/my scooter" "acp"))
           (words (split-string-shell-command
                   (string-join (flatten-tree (scooter-tramp-login-args)) " ")))
           (proxy (seq-find (lambda (w) (string-prefix-p "ProxyCommand=" w)) words)))
      ;; ssh runs the ProxyCommand through a second shell.
      (should (equal (split-string-shell-command (string-remove-prefix "ProxyCommand=" proxy))
                     '("/opt/my scooter" "tunnel" "--spec-file" "/tmp/a dir/%u.%h.json")))
      (should (member "/tmp/a dir/id_ed25519" words))
      (should (equal (last words) '("%h"))))))

(ert-deftest scooter-test-shell-find-file-starts-in-the-sandbox ()
  (let ((scooter-servers '((home :url "https://s.example") (me :url "https://m.example" :self t)))
        (calls 0))
    (cl-letf (((symbol-function 'scooter-tramp-file-name)
               (lambda (server id) (cl-incf calls) (format "/scooter:%s@%s:/workspace/" id server))))
      (with-temp-buffer
        (setq-local agent-shell--state '((:agent-config (:identifier . scooter-home))
                                         (:session (:id . "abc"))))
        (scooter-agent-shell--enable-files)
        (should scooter-agent-shell-files-mode)
        (should (eq (key-binding [remap find-file]) #'scooter-agent-shell-find-file))
        (should (equal (scooter-agent-shell-directory) "/scooter:abc@home:/workspace/"))
        (scooter-agent-shell-directory)
        (should (= calls 1))
        (cl-letf (((symbol-function 'find-file) #'identity))
          (should (equal (call-interactively #'scooter-find-file) "/scooter:abc@home:/workspace/"))))
      (with-temp-buffer
        (setq-local agent-shell--state '((:agent-config (:identifier . scooter-home))))
        (should-not (scooter-agent-shell-directory)))
      (with-temp-buffer
        (setq-local agent-shell--state '((:agent-config (:identifier . scooter-me))
                                         (:session (:id . "abc"))))
        (should-not (scooter-agent-shell-directory)))
      (with-temp-buffer
        (setq-local agent-shell--state '((:agent-config (:identifier . claude-code))))
        (scooter-agent-shell--enable-files)
        (should-not scooter-agent-shell-files-mode)))))

(defmacro scooter-test--in-shell (&rest body)
  "Run BODY in a Scooter shell for conversation abc on server home."
  (declare (indent 0))
  `(let ((scooter-servers '((home :url "https://s.example"))))
     (cl-letf (((symbol-function 'scooter-tramp-file-name)
                (lambda (server id) (format "/scooter:%s@%s:/workspace/" id server))))
       (with-temp-buffer
         (setq-local agent-shell--state '((:agent-config (:identifier . scooter-home))
                                          (:session (:id . "abc"))))
         (scooter-agent-shell--enable-files)
         ,@body))))

(defun scooter-test--links ()
  "Return this buffer's sandbox links as (TEXT PATH LINE COLUMN)."
  (mapcar (lambda (ov) (cons (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                             (overlay-get ov 'scooter-path)))
          (sort (seq-filter (lambda (ov) (overlay-get ov 'scooter-path))
                            (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(ert-deftest scooter-test-shell-links-sandbox-paths ()
  (scooter-test--in-shell
    ;; Text present when the mode starts, then text streamed in later.
    (insert "See /workspace/foo.mjs:42 and `/workspace/a/b.txt`.\n")
    (scooter-agent-shell-files-mode 1)
    (goto-char (point-max))
    (insert "wrote /workspace/x.go:3:7, /workspace/y.el#L9; not /home/workspace/z or /workspacefoo.\n")
    (insert "cd /workspace\n")
    (should (equal (scooter-test--links)
                   '(("/workspace/foo.mjs:42" "/workspace/foo.mjs" 42 nil)
                     ("/workspace/a/b.txt" "/workspace/a/b.txt" nil nil)
                     ("/workspace/x.go:3:7" "/workspace/x.go" 3 7)
                     ("/workspace/y.el#L9" "/workspace/y.el" 9 nil)
                     ("/workspace" "/workspace" nil nil))))))

(ert-deftest scooter-test-shell-link-grows-as-it-streams ()
  (scooter-test--in-shell
    (scooter-agent-shell-files-mode 1)
    (insert "at /workspace/foo.mjs:4")
    (insert "2 now")
    (should (equal (scooter-test--links) '(("/workspace/foo.mjs:42" "/workspace/foo.mjs" 42 nil))))
    (scooter-agent-shell-files-mode -1)
    (should-not (scooter-test--links))))

(ert-deftest scooter-test-shell-link-opens-the-sandbox-file ()
  (scooter-test--in-shell
    (scooter-agent-shell-files-mode 1)
    (insert "edited /workspace/foo.mjs:42:5 just now")
    (goto-char (point-min))
    (search-forward "foo")
    (should (eq (key-binding (kbd "RET")) #'scooter-agent-shell-open-path-at-point))
    (let (visited)
      (cl-letf (((symbol-function 'agent-shell-markdown-visit-file)
                 (lambda (&rest args) (setq visited args))))
        (scooter-agent-shell-open-path-at-point))
      (should (equal visited '(:file "/scooter:abc@home:/workspace/foo.mjs" :line-start 42 :column 5))))
    (goto-char (point-min))
    (should-error (scooter-agent-shell-open-path-at-point) :type 'user-error)))

(ert-deftest scooter-test-shell-file-keys ()
  (scooter-test--in-shell
    (should (eq (key-binding (kbd "C-c C-f")) #'scooter-agent-shell-find-file))
    (should (eq (key-binding (kbd "C-c C-j")) #'scooter-agent-shell-dired))
    (cl-letf (((symbol-function 'dired) #'identity))
      (should (equal (call-interactively #'scooter-agent-shell-dired) "/scooter:abc@home:/workspace/")))
    ;; The keymap is the user's to rebind.
    (keymap-set scooter-agent-shell-files-mode-map "C-c d" #'scooter-agent-shell-dired)
    (unwind-protect
        (should (eq (key-binding (kbd "C-c d")) #'scooter-agent-shell-dired))
      (keymap-unset scooter-agent-shell-files-mode-map "C-c d" t)))
  (with-temp-buffer
    (setq-local agent-shell--state '((:agent-config (:identifier . scooter-home))))
    (let ((scooter-servers '((home :url "https://s.example"))))
      (should-error (scooter-agent-shell-dired) :type 'user-error))))

(ert-deftest scooter-test-shell-renamed-after-title ()
  (let ((scooter-servers '((home :url "https://s.example")))
        (title nil))
    (cl-letf (((symbol-function 'scooter-request)
               (lambda (_server _method path)
                 (should (equal path "/conversations/abcdef123456"))
                 `((id . "abcdef123456") (title . ,title))))
              ((symbol-function 'agent-shell--project-name) (lambda () "proj"))
              ((symbol-function 'agent-shell--format-buffer-name)
               (lambda (name project) (format "%s Agent @ %s" name project))))
      (with-temp-buffer
        (rename-buffer "Scooter abcdef12@home Agent @ proj" t)
        (setq-local agent-shell--state
                    '((:agent-config (:identifier . scooter-home) (:buffer-name . "Scooter abcdef12@home"))
                      (:session (:id . "abcdef123456"))))
        (scooter-agent-shell-retitle)
        (should (equal (buffer-name) "Scooter abcdef12@home Agent @ proj"))
        (setq title "Fix the Login redirect")
        (scooter-agent-shell-retitle)
        (should (equal (buffer-name) "Scooter fix-the-login-redirect@home Agent @ proj"))
        (should (equal (map-nested-elt agent-shell--state '(:agent-config :buffer-name))
                       "Scooter fix-the-login-redirect@home"))
        (rename-buffer "mine" t)
        (setq title "Something else")
        (scooter-agent-shell-retitle)
        (should (equal (buffer-name) "mine"))))))

(provide 'scooter-test)
;;; scooter-test.el ends here

;;;; Conversation commands

(defmacro scooter-test-with-requests (replies &rest body)
  "Run BODY with `scooter-request' answering from REPLIES, an alist of
\"METHOD PATH\" to reply; bind `calls' to the requests made, oldest first."
  (declare (indent 1))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'scooter-request)
                (lambda (_server method path &optional body)
                  (setq calls (append calls (list (list method path body))))
                  (cdr (assoc (concat method " " path) ,replies)))))
       ,@body)))

(ert-deftest scooter-test-sandbox-actions ()
  (require 'scooter-conversation)
  (let ((conv '((id . "c1") (title . "Fix") (status . "running") (visibility . "private")))
        (sizes '((sizes (small . t) (large . t)) (default . "small"))))
    (should (equal (scooter-conversation--actions conv sizes)
                   '("resize" "suspend" "restart" "compact" "rename" "star" "make shared" "browse" "files")))
    (should (equal (scooter-conversation--describe conv '((resources (limits (cpu . "2") (memory . "4Gi")))) '((ready . t)))
                   "Fix: running (ready), private, 2 CPU, 4Gi"))
    (scooter-test-with-requests nil
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "large"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (scooter-conversation--run 'home "c1" "resize" conv sizes)
        (scooter-conversation--run 'home "c1" "unstar" conv sizes)
        (scooter-conversation--run 'home "c1" "make shared" conv sizes))
      (should (equal calls '(("PATCH" "/conversations/c1/size" ((size . "large")))
                             ("PATCH" "/conversations/c1/starred" ((starred . :json-false)))
                             ("PATCH" "/conversations/c1/visibility" ((visibility . "shared") (confirm . t)))))))))

(ert-deftest scooter-test-sandbox-refuses-while-a-run-goes ()
  (require 'scooter-conversation)
  (scooter-test-with-requests nil
    (dolist (action '("suspend" "restart" "compact"))
      (should-error (scooter-conversation--run 'home "c1" action '((runStartedAt . 5)) nil) :type 'user-error))
    (should-not calls)))

(ert-deftest scooter-test-web-services-start-with-absolute-urls ()
  (require 'scooter-conversation)
  (let ((scooter-servers '((home :url "https://s.example"))))
    (scooter-test-with-requests
        '(("GET /conversations/c1/web-services?refresh=1"
           (services ((name . "app") (displayName . "App") (running . :false) (url . "/s/c1/app/")))))
      (cl-letf (((symbol-function 'completing-read) (lambda (_p table &rest _) (caar table)))
                ((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?s "start"))))
        (should (equal (alist-get 'url (car (scooter-conversation--services 'home "c1")))
                       "https://s.example/s/c1/app/"))
        (scooter-web-services 'home "c1"))
      (should (equal (car (last calls)) '("POST" "/conversations/c1/web-services/app/start" nil))))))

(ert-deftest scooter-test-links-copy ()
  (require 'scooter-conversation)
  (scooter-test-with-requests
      '(("GET /conversations/c1/links"
         (links ((source . "github") (resourceType . "pull_request") (url . "https://gh/pr/1"))
                ((source . "slack") (title . "no url")))))
    (let ((kill-ring nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p table &rest _)
                   (should (equal (mapcar #'car table) '("github: pull request")))
                   (caar table))))
        (scooter-links 'home "c1" t))
      (should (equal (car kill-ring) "https://gh/pr/1")))))

(ert-deftest scooter-test-conversation-target-in-the-list ()
  (require 'scooter-conversation)
  (cl-letf (((symbol-function 'scooter-agent-shell-conversation) #'ignore)
            ((symbol-function 'scooter-list--at-point) (lambda () '(home (id . "c9")))))
    (with-temp-buffer
      (scooter-list-mode)
      (should (equal (scooter-conversation-target) '(home "c9"))))))

;;;; Browser sign-in through the Firefox profile

(defun scooter-test--add-cookie (db host name value)
  "Insert cookie NAME=VALUE for HOST into the moz_cookies DB."
  (sqlite-execute db "INSERT OR REPLACE INTO moz_cookies
 (originAttributes, name, value, host, path, expiry, lastAccessed, creationTime, isSecure, isHttpOnly)
 VALUES ('', ?, ?, ?, '/', 4102444800, 1, 1, 1, 1)"
                  (list name value host)))

(defmacro scooter-test-with-firefox (db-var &rest body)
  "Run BODY in a fake Firefox profile; DB-VAR is its open cookies.sqlite.
`scooter-auth-browsers' holds just this Firefox, as `firefox'; `root'
and `profile' name the directories.  The database stays open in WAL
mode, as a running Firefox keeps it, so fresh rows live in
cookies.sqlite-wal until a checkpoint."
  (declare (indent 1))
  `(let* ((root (make-temp-file "scooter-firefox" t))
          (profile (expand-file-name "abc123.default-release" root))
          (scooter-auth-browsers `((firefox :jar mozilla :directories (,root)
                                            :program browse-url-firefox-program)))
          (scooter-auth-browser-profile nil)
          (,db-var nil))
     (unwind-protect
         (progn
           (make-directory profile)
           (with-temp-file (expand-file-name "profiles.ini" root)
             (insert "[Profile1]\nName=old\nIsRelative=1\nPath=zzz.old\nDefault=1\n\n"
                     "[Profile0]\nName=default-release\nIsRelative=1\nPath=abc123.default-release\n\n"
                     "[Install4F96D1932A9F858E]\nDefault=abc123.default-release\nLocked=1\n"))
           (setq ,db-var (sqlite-open (expand-file-name "cookies.sqlite" profile)))
           (sqlite-pragma ,db-var "journal_mode=WAL")
           (sqlite-execute ,db-var "CREATE TABLE moz_cookies (id INTEGER PRIMARY KEY,
 originAttributes TEXT NOT NULL DEFAULT '', name TEXT, value TEXT, host TEXT, path TEXT,
 expiry INTEGER, lastAccessed INTEGER, creationTime INTEGER, isSecure INTEGER, isHttpOnly INTEGER,
 inBrowserElement INTEGER DEFAULT 0, sameSite INTEGER DEFAULT 0, rawSameSite INTEGER DEFAULT 0,
 schemeMap INTEGER DEFAULT 0, isPartitionedAttributeSet INTEGER DEFAULT 0,
 CONSTRAINT moz_uniqueid UNIQUE (name, host, path, originAttributes))")
           ,@body)
       (when ,db-var (sqlite-close ,db-var))
       (delete-directory root t))))

(defun scooter-test--firefox-jar (profile)
  "PROFILE's cookies.sqlite."
  (expand-file-name "cookies.sqlite" profile))

(ert-deftest scooter-test-firefox-default-profile ()
  (scooter-test-with-firefox _db
    ;; The [Install…] default wins over a Profile's Default=1.
    (should (equal (scooter-auth--mozilla-default-profile (list root))
                   (expand-file-name "abc123.default-release" root)))
    (with-temp-file (expand-file-name "profiles.ini" root)
      (insert "[Profile0]\nIsRelative=1\nPath=one\n\n[Profile1]\nIsRelative=0\nPath=/abs/two\nDefault=1\n"))
    (should (equal (scooter-auth--mozilla-default-profile (list root)) "/abs/two"))
    (should-not (scooter-auth--mozilla-default-profile (list (expand-file-name "missing" root))))))

(ert-deftest scooter-test-firefox-cookies-match-host-and-parents ()
  (scooter-test-with-firefox db
    (scooter-test--add-cookie db "other.example" "_oauth2_proxy" "other")
    (scooter-test--add-cookie db "xs.example" "_oauth2_proxy" "lookalike")
    (scooter-test--add-cookie db ".example" "_oauth2_proxy" "parent")
    (scooter-test--add-cookie db "s.example" "theme" "dark")
    (scooter-test--add-cookie db "s.example" "_oauth2_proxy" "exact")
    ;; Still in the WAL: only a copy that brings the -wal along sees them.
    (should (file-exists-p (expand-file-name "abc123.default-release/cookies.sqlite-wal" root)))
    (should (equal (scooter-auth--mozilla-cookies (scooter-test--firefox-jar profile) "s.example")
                   '("exact" "parent")))
    (should (equal (scooter-auth--mozilla-cookies (scooter-test--firefox-jar profile) "a.s.example")
                   '("parent")))
    (should-not (scooter-auth--mozilla-cookies (scooter-test--firefox-jar profile) "example.org"))))

(ert-deftest scooter-test-firefox-cookies-join-split-ones ()
  (scooter-test-with-firefox db
    ;; Inserted out of order; joined by index.
    (scooter-test--add-cookie db "s.example" "_oauth2_proxy_1" "b")
    (scooter-test--add-cookie db "s.example" "_oauth2_proxy_0" "a")
    (scooter-test--add-cookie db "s.example" "_oauth2_proxy_2" "c")
    (should (equal (scooter-auth--mozilla-cookies (scooter-test--firefox-jar profile) "s.example") '("abc")))
    ;; A whole cookie beside split parts wins.
    (scooter-test--add-cookie db "s.example" "_oauth2_proxy" "whole")
    (should (equal (scooter-auth--mozilla-cookies (scooter-test--firefox-jar profile) "s.example") '("whole")))))

(ert-deftest scooter-test-firefox-cookies-by-name ()
  ;; A --cookie-name server: only cookies of that name count, split ones
  ;; included; the default name's cookie belongs to some other server.
  (scooter-test-with-firefox db
    (scooter-test--add-cookie db "s.example" "_oauth2_proxy" "default")
    (scooter-test--add-cookie db "s.example" "__Host-scooter_oauth2" "whole")
    (scooter-test--add-cookie db ".example" "__Host-scooter_oauth2_1" "b")
    (scooter-test--add-cookie db ".example" "__Host-scooter_oauth2_0" "a")
    (should (equal (scooter-auth--mozilla-cookies (scooter-test--firefox-jar profile) "s.example"
                                                  "__Host-scooter_oauth2")
                   '("whole" "ab")))
    (should (equal (scooter-auth--mozilla-cookies (scooter-test--firefox-jar profile) "s.example")
                   '("default")))))

(ert-deftest scooter-test-firefox-profiles-default-first-then-the-rest ()
  (scooter-test-with-firefox _db
    (let ((other (expand-file-name "zzz.old" root))
          (stray (expand-file-name "stray" root))
          (firefox (car scooter-auth-browsers)))
      (make-directory other)
      (make-directory stray)
      (with-temp-file (expand-file-name "cookies.sqlite" other))
      (with-temp-file (expand-file-name "cookies.sqlite" stray))
      ;; Not profiles: no cookies.sqlite, or not in profiles.ini at all.
      (make-directory (expand-file-name "Crash Reports" root))
      (should (equal (scooter-auth--browser-profiles firefox)
                     (list (expand-file-name "abc123.default-release" root) other stray)))
      (let ((scooter-auth-browser-profile other))
        (should (equal (scooter-auth--browser-profiles firefox) (list other))))
      (let ((scooter-auth-browser-profile (expand-file-name "Crash Reports" root)))
        (should-not (scooter-auth--browser-profiles firefox))))))

;;;; Chromium's jar

(defconst scooter-test--chromium-schema
  "CREATE TABLE cookies (creation_utc INTEGER NOT NULL, host_key TEXT NOT NULL,
 top_frame_site_key TEXT NOT NULL, name TEXT NOT NULL, value TEXT NOT NULL,
 encrypted_value BLOB NOT NULL, path TEXT NOT NULL, expires_utc INTEGER NOT NULL,
 is_secure INTEGER NOT NULL, is_httponly INTEGER NOT NULL, last_access_utc INTEGER NOT NULL,
 has_expires INTEGER NOT NULL, is_persistent INTEGER NOT NULL, priority INTEGER NOT NULL,
 samesite INTEGER NOT NULL, source_scheme INTEGER NOT NULL, source_port INTEGER NOT NULL,
 last_update_utc INTEGER NOT NULL, source_type INTEGER NOT NULL,
 has_cross_site_ancestor INTEGER NOT NULL)"
  "Chromium 156's cookies table.")

(defun scooter-test--unhex (hex)
  "The bytes HEX spells, as a unibyte string."
  (apply #'unibyte-string
         (mapcar (lambda (i) (string-to-number (substring hex i (+ i 2)) 16))
                 (number-sequence 0 (- (length hex) 2) 2))))

(defun scooter-test--hex (bytes)
  "BYTES as hex."
  (mapconcat (lambda (b) (format "%02x" b)) bytes ""))

(defconst scooter-test--chromium-blob
  (scooter-test--unhex
   "7631302BB01EEBE40CCFF8B9D3B700273158B01EEE6BEDEECAB4DE6DBA0816A56B00385BC1E91200EA0A6319B9346027DAAE06")
  "What Chromium 156 (schema 24, basic password store) wrote for a cookie
on 127.0.0.1 whose value is \"part-a\".")

(defun scooter-test--chromium-encrypt (host value &optional version password)
  "Encrypt VALUE for HOST as a schema-24 Chromium would, VERSION v10 by default."
  (let* ((key (scooter-auth--chromium-key (or password "peanuts")))
         (text (concat (secure-hash 'sha256 host nil nil t) (encode-coding-string value 'utf-8)))
         (pad (- 16 (% (length text) 16))))
    (concat (or version "v10")
            (car (gnutls-symmetric-encrypt 'AES-128-CBC key (make-string 16 ?\s)
                                           (concat text (make-string pad pad)))))))

(defmacro scooter-test-with-chromium (db-var &rest body)
  "Run BODY in a fake Chromium user data directory, `root'.
DB-VAR is the Default profile's open Network/Cookies; `profile' is that
profile.  `scooter-auth-browsers' holds just this Chromium."
  (declare (indent 1))
  `(let* ((root (make-temp-file "scooter-chromium" t))
          (profile (expand-file-name "Default" root))
          (scooter-auth-browsers `((chromium :jar chromium :directories (,root)
                                             :program "chromium" :secret "Chromium Safe Storage")))
          (scooter-auth-browser-profile nil)
          (scooter-auth--safe-storage nil)
          (,db-var nil))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "Network" profile) t)
           (with-temp-file (expand-file-name "Local State" root)
             (insert "{\"profile\":{\"info_cache\":{\"Default\":{\"active_time\":1.0,\"name\":\"Me\"}},"
                     "\"last_used\":\"Default\"}}"))
           (setq ,db-var (sqlite-open (expand-file-name "Network/Cookies" profile)))
           (sqlite-execute ,db-var "CREATE TABLE meta (key LONGVARCHAR NOT NULL UNIQUE PRIMARY KEY, value LONGVARCHAR)")
           (sqlite-execute ,db-var "INSERT INTO meta VALUES ('version', '24')")
           (sqlite-execute ,db-var scooter-test--chromium-schema)
           ,@body)
       (when ,db-var (sqlite-close ,db-var))
       (delete-directory root t))))

(defun scooter-test--add-chromium-cookie (db host name value &optional plain version)
  "Insert NAME=VALUE for HOST into the Chromium DB, encrypted (as VERSION) unless PLAIN."
  (sqlite-execute
   db (format "INSERT INTO cookies (creation_utc, host_key, top_frame_site_key, name, value,
 encrypted_value, path, expires_utc, is_secure, is_httponly, last_access_utc, has_expires,
 is_persistent, priority, samesite, source_scheme, source_port, last_update_utc, source_type,
 has_cross_site_ancestor) VALUES (1, ?, '', ?, ?, X'%s', '/', 99999999999999999, 1, 1, 1, 1, 1, 1, 0, 2, 443, 1, 0, 0)"
              (if plain "" (scooter-test--hex (scooter-test--chromium-encrypt host value version))))
   (list host name (if plain value ""))))

(defun scooter-test--chromium-jar (profile)
  "PROFILE's cookie database."
  (expand-file-name "Network/Cookies" profile))

(ert-deftest scooter-test-chromium-decrypts-what-chromium-wrote ()
  (let ((blob scooter-test--chromium-blob))
    (should (equal (scooter-auth--chromium-decrypt blob "127.0.0.1" 24 nil) "part-a"))
    ;; Schema 24 binds the value to its host: another host's hash doesn't match.
    (should-not (scooter-auth--chromium-decrypt blob "evil.example" 24 nil))
    ;; Our encryption round-trips the same way.
    (should (equal (scooter-auth--chromium-decrypt (scooter-test--chromium-encrypt "s.example" "héllo")
                                                   "s.example" 24 nil)
                   "héllo"))
    ;; Keyring-encrypted: readable through the browser's Safe Storage item only.
    (let ((v11 (concat "v11" (substring blob 3)))
          (scooter-auth--safe-storage nil))
      (cl-letf (((symbol-function 'scooter-auth--safe-storage-password)
                 (lambda (label) (and (equal label "Chromium Safe Storage") "peanuts"))))
        (should (equal (scooter-auth--chromium-decrypt v11 "127.0.0.1" 24 "Chromium Safe Storage")
                       "part-a"))
        (should-not (scooter-auth--chromium-decrypt v11 "127.0.0.1" 24 "Brave Safe Storage"))))
    ;; Not a blob at all.
    (should-not (scooter-auth--chromium-decrypt "" "127.0.0.1" 24 nil))
    (should-not (scooter-auth--chromium-decrypt "v10abc" "127.0.0.1" 24 nil))))

(ert-deftest scooter-test-chromium-cookies-by-host-and-name ()
  (scooter-test-with-chromium db
    (scooter-test--add-chromium-cookie db "s.example" "_oauth2_proxy" "exact")
    (scooter-test--add-chromium-cookie db ".example" "_oauth2_proxy" "parent")
    (scooter-test--add-chromium-cookie db "xs.example" "_oauth2_proxy" "lookalike")
    (scooter-test--add-chromium-cookie db "s.example" "__Host-x" "whole")
    (scooter-test--add-chromium-cookie db "s.example" "__Host-y_1" "b")
    (scooter-test--add-chromium-cookie db "s.example" "__Host-y_0" "a")
    (scooter-test--add-chromium-cookie db "s.example" "plain" "clear" t)
    (scooter-test--add-chromium-cookie db "s.example" "__Host-z" "keyring" nil "v11")
    (let ((jar (scooter-test--chromium-jar profile)))
      (should (equal (scooter-auth--chromium-cookies jar "s.example") '("exact" "parent")))
      (should (equal (scooter-auth--chromium-cookies jar "s.example" "__Host-x") '("whole")))
      (should (equal (scooter-auth--chromium-cookies jar "s.example" "__Host-y") '("ab")))
      (should (equal (scooter-auth--chromium-cookies jar "s.example" "plain") '("clear")))
      (should-not (scooter-auth--chromium-cookies jar "example.org"))
      ;; v11 without the keyring: counted, not returned; with it, read.
      (setq scooter-auth--undecryptable 0)
      (cl-letf (((symbol-function 'scooter-auth--safe-storage-password) #'ignore))
        (should-not (scooter-auth--chromium-cookies jar "s.example" "__Host-z" "Chromium Safe Storage"))
        (should (= scooter-auth--undecryptable 1)))
      (cl-letf (((symbol-function 'scooter-auth--safe-storage-password)
                 (lambda (label) (and (equal label "Chromium Safe Storage") "peanuts"))))
        (should (equal (scooter-auth--chromium-cookies jar "s.example" "__Host-z" "Chromium Safe Storage")
                       '("keyring")))))))

(ert-deftest scooter-test-chromium-profiles-last-used-first ()
  (scooter-test-with-chromium _db
    (let ((chromium (car scooter-auth-browsers))
          (p1 (expand-file-name "Profile 1" root))
          (stray (expand-file-name "Guest" root))
          (state (expand-file-name "Local State" root)))
      (make-directory (expand-file-name "Network" p1) t)
      (with-temp-file (expand-file-name "Network/Cookies" p1))
      (make-directory stray)
      (with-temp-file (expand-file-name "Cookies" stray))
      ;; No jar: not a profile.
      (make-directory (expand-file-name "Crashpad" root))
      (with-temp-file state
        (insert "{\"profile\":{\"info_cache\":{\"Default\":{\"active_time\":1.0},"
                "\"Profile 1\":{\"active_time\":5.0}},\"last_used\":\"Default\"}}"))
      (should (equal (scooter-auth--browser-profiles chromium) (list profile p1 stray)))
      ;; Without last_used, the most recently active profile leads.
      (with-temp-file state
        (insert "{\"profile\":{\"info_cache\":{\"Default\":{\"active_time\":1.0},"
                "\"Profile 1\":{\"active_time\":5.0}}}}"))
      (should (equal (scooter-auth--browser-profiles chromium) (list p1 profile stray)))
      ;; No Local State at all: whatever holds a jar.
      (delete-file state)
      (should (equal (sort (scooter-auth--browser-profiles chromium) #'string<)
                     (sort (list profile p1 stray) #'string<)))
      (let ((scooter-auth-browser-profile p1))
        (should (equal (scooter-auth--browser-profiles chromium) (list p1)))))))

;;;; Signing in through a browser

(ert-deftest scooter-test-browser-login-reads-the-named-cookie ()
  ;; Firefox has a good cookie under the default name, but the server's is
  ;; __Host-scooter_oauth2: the sign-in waits for that one and sends it.
  (scooter-test-with-fake-server url
    (scooter-test-with-firefox db
      (let ((scooter-servers `((home :url ,url :auth (cookie :browser firefox
                                                             :cookie-name "__Host-scooter_oauth2"))))
            (scooter-auth--cache (make-hash-table :test #'eq))
            (scooter-auth-browser-poll-interval 0.1)
            saved opened)
        (scooter-test--add-cookie db "127.0.0.1" "_oauth2_proxy" "good")
        (scooter-test--add-cookie db "127.0.0.1" "__Host-scooter_oauth2" "expired")
        (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
                  ((symbol-function 'scooter-auth--save-secret)
                   (lambda (_host user secret) (push (cons user secret) saved)))
                  ((symbol-function 'read-passwd) (lambda (&rest _) (error "Asked to paste")))
                  ((symbol-function 'scooter-auth--browse)
                   (lambda (_browsers u)
                     (push u opened)
                     (run-at-time 0.3 nil (lambda () (scooter-test--add-cookie
                                                      db "127.0.0.1" "__Host-scooter_oauth2" "good"))))))
          (should (equal (scooter-auth-headers 'home t)
                         '(("Cookie" . "__Host-scooter_oauth2=good"))))
          (should (equal opened (list (concat url "/oauth2/start?rd=%2F"))))
          (should (equal saved '(("__Host-scooter_oauth2" . "good")))))))))

(ert-deftest scooter-test-browser-login-reuses-the-firefox-session ()
  (scooter-test-with-fake-server url
    (scooter-test-with-firefox db
      (let ((scooter-servers `((home :url ,url :auth (cookie :browser firefox))))
            (scooter-auth--cache (make-hash-table :test #'eq))
            saved opened)
        (scooter-test--add-cookie db "127.0.0.1" "_oauth2_proxy" "good")
        (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
                  ((symbol-function 'scooter-auth--save-secret)
                   (lambda (_host user secret) (push (cons user secret) saved)))
                  ((symbol-function 'scooter-auth--browse) (lambda (_b u) (push u opened)))
                  ((symbol-function 'read-passwd) (lambda (&rest _) (error "Asked to paste"))))
          (should (equal (scooter-auth-headers 'home) '(("Cookie" . "_oauth2_proxy=good"))))
          (should-not opened)
          (should (equal saved '(("_oauth2_proxy" . "good")))))))))

(ert-deftest scooter-test-browser-login-opens-the-sign-in-page-and-polls ()
  (scooter-test-with-fake-server url
    (scooter-test-with-firefox db
      (let ((scooter-servers `((home :url ,url :auth (cookie :browser firefox))))
            (scooter-auth--cache (make-hash-table :test #'eq))
            (scooter-auth-browser-poll-interval 0.1)
            opened nested)
        (scooter-test--add-cookie db "127.0.0.1" "_oauth2_proxy" "expired")
        (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
                  ((symbol-function 'scooter-auth--save-secret) #'ignore)
                  ((symbol-function 'read-passwd) (lambda (&rest _) (error "Asked to paste")))
                  ((symbol-function 'scooter-auth--browse)
                   (lambda (_browsers u)
                     (push u opened)
                     ;; The user signs in a little later.
                     (run-at-time 0.3 nil (lambda () (scooter-test--add-cookie
                                                      db "127.0.0.1" "_oauth2_proxy" "good")))
                     ;; Another shell's bridge asks meanwhile: told to wait.
                     (run-at-time 0.1 nil (lambda () (push (scooter-auth-headers-json "home" t) nested))))))
          (should (equal (scooter-auth-headers 'home t) '(("Cookie" . "_oauth2_proxy=good"))))
          (should (equal opened (list (concat url "/oauth2/start?rd=%2F"))))
          (should (equal nested '("null")))
          (should-not (scooter-auth--get 'home :signing-in))
          (should (equal (scooter-auth-headers-json "home") "{\"Cookie\":\"_oauth2_proxy=good\"}")))))))

(ert-deftest scooter-test-browser-login-times-out ()
  (scooter-test-with-fake-server url
    (scooter-test-with-firefox _db
      (let ((scooter-servers `((home :url ,url :auth (cookie :browser firefox))))
            (scooter-auth--cache (make-hash-table :test #'eq))
            (scooter-auth-login-timeout 1)
            (scooter-auth-browser-poll-interval 0.1))
        (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
                  ((symbol-function 'scooter-auth--browse) #'ignore))
          (should (string-match-p "not finished"
                                  (cadr (should-error (scooter-auth-headers 'home t) :type 'user-error))))
          (should-not (scooter-auth--get 'home :signing-in))
          (scooter-auth--forget-failures))))))

(ert-deftest scooter-test-failed-sign-in-waits-once-per-command ()
  ;; One command fetching twice (boards, then user skills) gets one wait;
  ;; the next command tries again.  Background work never waits.
  (scooter-test-with-fake-server url
    (scooter-test-with-firefox _db
      (let ((scooter-servers `((home :url ,url :auth (cookie :browser firefox))))
            (scooter-auth--cache (make-hash-table :test #'eq))
            (scooter-auth-login-timeout 0.3)
            (scooter-auth-browser-poll-interval 0.1)
            (opened 0))
        (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
                  ((symbol-function 'scooter-auth--browse) (lambda (&rest _) (cl-incf opened))))
          (unwind-protect
              (progn
                (should (string-match-p "not finished"
                                        (cadr (should-error (scooter-auth-headers 'home) :type 'user-error))))
                (should (string-match-p "not finished.*tries again"
                                        (cadr (should-error (scooter-auth-headers 'home) :type 'user-error))))
                (should (= opened 1))
                (let ((scooter-auth-quiet t))
                  (should-error (scooter-auth-headers 'home) :type 'scooter-auth-needed))
                (should (= opened 1))
                ;; The next command starts afresh.
                (run-hooks 'pre-command-hook)
                (should-not (memq #'scooter-auth--forget-failures pre-command-hook))
                (should-error (scooter-auth-headers 'home) :type 'user-error)
                (should (= opened 2))
                ;; C-g during the wait counts the same way.
                (run-hooks 'pre-command-hook)
                (cl-letf (((symbol-function 'accept-process-output)
                           (lambda (&rest _) (signal 'quit nil))))
                  ;; `should-error' handles `error' only; `quit' isn't one.
                  (should (eq (condition-case nil (progn (scooter-auth-headers 'home) 'returned)
                                (quit 'quit))
                              'quit)))
                (should (= opened 3))
                (should (equal (scooter-auth--get 'home :sign-in-failed) "sign-in cancelled"))
                (should-not (scooter-auth--get 'home :signing-in))
                (should (string-match-p "cancelled"
                                        (cadr (should-error (scooter-auth-headers 'home) :type 'user-error))))
                (should (= opened 3))
                ;; The explicit command always tries.
                (cl-letf (((symbol-function 'scooter-current-server) (lambda (&rest _) 'home)))
                  (should-error (call-interactively #'scooter-auth-browser-login) :type 'user-error))
                (should (= opened 4)))
            (scooter-auth--forget-failures)))))))

(ert-deftest scooter-test-quiet-auth-uses-only-what-it-has ()
  ;; A cached or stored cookie serves quietly; nothing else does.
  (let ((scooter-servers '((home :url "https://s.example" :auth cookie)))
        (scooter-auth--cache (make-hash-table :test #'eq))
        (scooter-auth-quiet t))
    (cl-letf (((symbol-function 'scooter-auth--secret) (lambda (&rest _) "stored"))
              ((symbol-function 'read-passwd) (lambda (&rest _) (error "Asked to paste")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked to browse"))))
      (should (equal (scooter-auth-headers 'home) '(("Cookie" . "_oauth2_proxy=stored"))))
      (should-error (scooter-auth-headers 'home t) :type 'scooter-auth-needed))))

(ert-deftest scooter-test-browser-login-without-firefox-never-asks-to-paste ()
  (let ((scooter-servers '((home :url "https://s.example" :auth (cookie :browser firefox))))
        (scooter-auth--cache (make-hash-table :test #'eq))
        (scooter-auth-browsers '((firefox :jar mozilla :directories ("/nonexistent/firefox"))))
        (scooter-auth-browser-profile nil))
    (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
              ((symbol-function 'scooter-auth--save-secret) #'ignore)
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked to open a browser")))
              ((symbol-function 'read-passwd) (lambda (&rest _) (error "Asked to paste"))))
      (should (string-match-p "no Firefox profile"
                              (cadr (should-error (scooter-auth-headers 'home t) :type 'user-error))))
      (should-not (scooter-auth--get 'home :signing-in))
      (let ((scooter-servers '((home :url "https://s.example" :auth (cookie :browser lynx)))))
        (should (string-match-p "unknown :browser lynx"
                                (cadr (should-error (scooter-auth-headers 'home t) :type 'user-error))))))))

(ert-deftest scooter-test-browser-login-finds-the-cookie-in-another-profile ()
  ;; Signed in in a profile other than the default: still no prompt.
  (scooter-test-with-fake-server url
    (scooter-test-with-firefox _db
      (let* ((scooter-servers `((home :url ,url :auth (cookie :browser firefox))))
             (scooter-auth--cache (make-hash-table :test #'eq))
             (other (expand-file-name "zzz.old" root))
             (db2 nil)
             opened)
        (make-directory other)
        (unwind-protect
            (progn
              (setq db2 (sqlite-open (expand-file-name "cookies.sqlite" other)))
              (sqlite-execute db2 "CREATE TABLE moz_cookies (id INTEGER PRIMARY KEY,
 originAttributes TEXT NOT NULL DEFAULT '', name TEXT, value TEXT, host TEXT, path TEXT,
 expiry INTEGER, lastAccessed INTEGER, creationTime INTEGER, isSecure INTEGER, isHttpOnly INTEGER,
 CONSTRAINT moz_uniqueid UNIQUE (name, host, path, originAttributes))")
              (scooter-test--add-cookie db2 "127.0.0.1" "_oauth2_proxy" "good")
              (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
                        ((symbol-function 'scooter-auth--save-secret) #'ignore)
                        ((symbol-function 'scooter-auth--browse) (lambda (_b u) (push u opened)))
                        ((symbol-function 'read-passwd) (lambda (&rest _) (error "Asked to paste"))))
                (should (equal (scooter-auth-headers 'home t) '(("Cookie" . "_oauth2_proxy=good"))))
                (should-not opened)))
          (when db2 (sqlite-close db2)))))))

(ert-deftest scooter-test-any-browser-reads-every-jar ()
  ;; :browser t: the sign-in page opens with `browse-url', and whichever
  ;; browser gets the session serves, Chromium here while Firefox's is stale.
  (scooter-test-with-fake-server url
    (scooter-test-with-firefox fdb
      (let ((firefox (car scooter-auth-browsers)))
        (scooter-test-with-chromium cdb
          (let ((scooter-auth-browsers (cons firefox scooter-auth-browsers))
                (scooter-servers `((home :url ,url :auth (cookie :browser t))))
                (scooter-auth--cache (make-hash-table :test #'eq))
                (scooter-auth-browser-poll-interval 0.1)
                opened)
            (scooter-test--add-cookie fdb "127.0.0.1" "_oauth2_proxy" "expired")
            (cl-letf (((symbol-function 'scooter-auth--secret) #'ignore)
                      ((symbol-function 'scooter-auth--save-secret) #'ignore)
                      ((symbol-function 'read-passwd) (lambda (&rest _) (error "Asked to paste")))
                      ((symbol-function 'browse-url-generic) (lambda (&rest _) (push 'generic opened)))
                      ((symbol-function 'browse-url)
                       (lambda (u &rest _)
                         (push u opened)
                         (run-at-time 0.3 nil (lambda () (scooter-test--add-chromium-cookie
                                                          cdb "127.0.0.1" "_oauth2_proxy" "good"))))))
              (should (equal (scooter-auth-headers 'home t) '(("Cookie" . "_oauth2_proxy=good"))))
              (should (equal opened (list (concat url "/oauth2/start?rd=%2F"))))
              ;; Firefox alone: Chromium's session doesn't count, and the
              ;; page opens in Firefox itself.
              (let ((scooter-servers `((home :url ,url :auth (cookie :browser firefox))))
                    (scooter-auth--cache (make-hash-table :test #'eq))
                    (scooter-auth-login-timeout 0.3))
                (setq opened nil)
                (cl-letf (((symbol-function 'executable-find)
                           (lambda (program &rest _) (equal program "firefox"))))
                  (should-error (scooter-auth-headers 'home t) :type 'user-error))
                (should (equal opened '(generic)))
                (scooter-auth--forget-failures))
              ;; Chromium alone, Firefox's cookie meanwhile good: Chromium's wins anyway.
              (let ((scooter-servers `((home :url ,url :auth (cookie :browser chromium))))
                    (scooter-auth--cache (make-hash-table :test #'eq)))
                (setq opened nil)
                (should (equal (scooter-auth-headers 'home t) '(("Cookie" . "_oauth2_proxy=good"))))
                (should-not opened)))))))))

