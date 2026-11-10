;;; scooter-kanban-edit.el --- Edit Scooter's kanban boards  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Chad Crawford
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Commands that change a board itself: make, rename and delete boards;
;; add, rename, reorder and delete its stages and tags; write its skills,
;; the notes every agent on the board reads.  A board's stages, tags and
;; skills save together, so each command sends the whole layout as it
;; should end up, as the web UI's Edit board does.
;;
;; Also the user's personal skills (the settings page's Skills: a name,
;; when to use it, and markdown instructions) and the deployment's hooks,
;; which they can turn off.  A skill, personal or a board's, is edited as
;; a file: front matter with its name and details over the markdown body,
;; in a buffer of its own; C-c C-c submits it.  A card's description is
;; the same buffer without the front matter.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'url-util)
(require 'scooter-kanban)

;;;; Boards

;;;###autoload
(defun scooter-kanban-new-board (server name)
  "Make a board called NAME on SERVER, with kanban's default stages."
  (interactive (list (scooter-kanban-server-at-point) (read-string "New board: ")))
  (scooter-kanban-request server "POST" "boards" `((name . ,name)))
  (scooter-kanban--after-change))

;;;###autoload
(defun scooter-kanban-rename-board (name)
  "Rename the board at point to NAME."
  (interactive (list (read-string "Rename board: " (plist-get (scooter-kanban-board-at-point) :name))))
  (let ((board (scooter-kanban-board-at-point)))
    (scooter-kanban-request (plist-get board :server) "PATCH"
                            (scooter-kanban--board-path (plist-get board :id)) `((name . ,name)))
    (scooter-kanban--after-change)))

;;;###autoload
(defun scooter-kanban-delete-board ()
  "Delete the board at point; its conversations stay, unfiled."
  (interactive)
  (let ((board (scooter-kanban-board-at-point)))
    (when (yes-or-no-p (format "Delete the board %s? Its %d conversations stay, unfiled. "
                               (plist-get board :name) (length (plist-get board :cards))))
      (scooter-kanban-request (plist-get board :server) "DELETE"
                              (scooter-kanban--board-path (plist-get board :id)))
      (scooter-kanban--after-change))))

;;;; Layout: stages, tags and skills

(defun scooter-kanban-edit--layout (board)
  "Return BOARD's stages, tags and skills as a PUT body would carry them."
  (list :stages (mapcar (lambda (s) `((id . ,(alist-get 'id s)) (name . ,(alist-get 'label s))))
                        (plist-get board :stages))
        :tags (mapcar (lambda (tag) `((id . ,(alist-get 'id tag)) (name . ,(alist-get 'name tag))
                                      (color . ,(alist-get 'color tag))))
                      (plist-get board :tags))
        :skills (mapcar (lambda (k) `((id . ,(alist-get 'id k)) (name . ,(alist-get 'name k))
                                      (body . ,(alist-get 'body k))))
                        (plist-get board :skills))))

(defun scooter-kanban-edit--save (board change)
  "Save BOARD's layout after CHANGE, a function of the layout plist."
  (let ((layout (funcall change (scooter-kanban-edit--layout board))))
    (scooter-kanban-request (plist-get board :server) "PUT"
                            (scooter-kanban--board-path (plist-get board :id))
                            `((stages . ,(vconcat (plist-get layout :stages)))
                              (tags . ,(vconcat (plist-get layout :tags)))
                              (skills . ,(vconcat (plist-get layout :skills)))))
    (scooter-kanban--after-change)))

(defun scooter-kanban-edit--swap (list i j)
  "Return LIST with the items at I and J swapped."
  (unless (and (<= 0 i) (< i (length list)) (<= 0 j) (< j (length list)))
    (user-error "Nothing to swap with"))
  (let ((v (vconcat list)))
    (cl-rotatef (aref v i) (aref v j))
    (append v nil)))

(defun scooter-kanban-edit--index (items id)
  "Return the index of the item with ID in ITEMS."
  (or (seq-position items id (lambda (item id) (equal (alist-get 'id item) id)))
      (user-error "No such item")))

(defun scooter-kanban-edit--update (items id key value)
  "Return ITEMS with the one whose id is ID having KEY set to VALUE."
  (mapcar (lambda (item)
            (if (equal (alist-get 'id item) id)
                (cons (cons key value) (assq-delete-all key (copy-alist item)))
              item))
          items))

(defun scooter-kanban-edit--at (class board key prompt)
  "Return the id of the CLASS section at point, or read one of BOARD's KEY items.
PROMPT reads it by name."
  (let ((s (magit-current-section)))
    (if (cl-typep s class)
        (oref s value)
      (let ((table (mapcar (lambda (item) (cons (or (alist-get 'label item) (alist-get 'name item))
                                                (alist-get 'id item)))
                           (plist-get board key))))
        (unless table (user-error "This board has none"))
        (cdr (assoc (completing-read prompt table nil t) table))))))

(defun scooter-kanban-edit--stage (board)
  "Return the stage id at point on BOARD, or read one."
  (scooter-kanban-stage-at-point board))

(defun scooter-kanban-add-stage (name)
  "Add a stage NAME to the board at point, after the stage at point or last."
  (interactive (list (read-string "New stage: ")))
  (let* ((board (scooter-kanban-board-at-point))
         (s (magit-current-section))
         (after (and (cl-typep s 'scooter-kanban-stage-section) (oref s value))))
    (scooter-kanban-edit--save
     board (lambda (l)
             (let* ((stages (plist-get l :stages))
                    (at (if after (1+ (scooter-kanban-edit--index stages after)) (length stages))))
               (plist-put l :stages (append (seq-take stages at) (list `((name . ,name)))
                                            (seq-drop stages at))))))))

(defun scooter-kanban-rename-stage (name)
  "Rename the stage at point to NAME."
  (interactive (let ((board (scooter-kanban-board-at-point)))
                 (list (read-string "Rename stage: "
                                    (scooter-kanban-stage-name board (scooter-kanban-edit--stage board))))))
  (let* ((board (scooter-kanban-board-at-point))
         (stage (scooter-kanban-edit--stage board)))
    (scooter-kanban-edit--save
     board (lambda (l) (plist-put l :stages (scooter-kanban-edit--update (plist-get l :stages) stage 'name name))))))

(defun scooter-kanban-delete-stage ()
  "Delete the stage at point; its cards move to the first stage."
  (interactive)
  (let* ((board (scooter-kanban-board-at-point))
         (stage (scooter-kanban-edit--stage board)))
    (when (<= (length (plist-get board :stages)) 1) (user-error "A board needs a stage"))
    (when (yes-or-no-p (format "Delete the stage %s? Its %d cards move to the first stage. "
                               (scooter-kanban-stage-name board stage)
                               (length (scooter-kanban-lane board stage))))
      (scooter-kanban-edit--save
       board (lambda (l) (plist-put l :stages (seq-remove (lambda (s) (equal (alist-get 'id s) stage))
                                                          (plist-get l :stages))))))))

(defun scooter-kanban-edit--shift (key class prompt by)
  "Move the KEY item at point (a CLASS section, else read with PROMPT) BY places."
  (let* ((board (scooter-kanban-board-at-point))
         (id (if (eq key :stages) (scooter-kanban-edit--stage board)
               (scooter-kanban-edit--at class board key prompt))))
    (scooter-kanban-edit--save
     board (lambda (l)
             (let* ((items (plist-get l key))
                    (i (scooter-kanban-edit--index items id)))
               (plist-put l key (scooter-kanban-edit--swap items i (+ i by))))))))

(defun scooter-kanban-stage-left ()
  "Move the stage at point one place earlier."
  (interactive)
  (scooter-kanban-edit--shift :stages nil nil -1))

(defun scooter-kanban-stage-right ()
  "Move the stage at point one place later."
  (interactive)
  (scooter-kanban-edit--shift :stages nil nil 1))

(defun scooter-kanban-add-tag (name color)
  "Add a tag NAME in COLOR to the board at point."
  (interactive (list (read-string "New tag: ")
                     (completing-read "Color: " (mapcar #'car scooter-kanban-tag-colors) nil t)))
  (scooter-kanban-edit--save
   (scooter-kanban-board-at-point)
   (lambda (l) (plist-put l :tags (append (plist-get l :tags) (list `((name . ,name) (color . ,color))))))))

(defun scooter-kanban-edit--tag (board)
  "Return the tag id at point on BOARD, or read one."
  (scooter-kanban-edit--at 'scooter-kanban-tag-section board :tags "Tag: "))

(defun scooter-kanban-rename-tag ()
  "Rename the tag at point."
  (interactive)
  (let* ((board (scooter-kanban-board-at-point))
         (tag (scooter-kanban-edit--tag board))
         (old (alist-get 'name (seq-find (lambda (x) (equal (alist-get 'id x) tag)) (plist-get board :tags))))
         (name (read-string "Rename tag: " old)))
    (scooter-kanban-edit--save
     board (lambda (l) (plist-put l :tags (scooter-kanban-edit--update (plist-get l :tags) tag 'name name))))))

(defun scooter-kanban-color-tag ()
  "Change the color of the tag at point."
  (interactive)
  (let* ((board (scooter-kanban-board-at-point))
         (tag (scooter-kanban-edit--tag board))
         (color (completing-read "Color: " (mapcar #'car scooter-kanban-tag-colors) nil t)))
    (scooter-kanban-edit--save
     board (lambda (l) (plist-put l :tags (scooter-kanban-edit--update (plist-get l :tags) tag 'color color))))))

(defun scooter-kanban-delete-tag ()
  "Delete the tag at point, taking it off every card."
  (interactive)
  (let* ((board (scooter-kanban-board-at-point))
         (tag (scooter-kanban-edit--tag board)))
    (when (yes-or-no-p "Delete this tag from the board and its cards? ")
      (scooter-kanban-edit--save
       board (lambda (l) (plist-put l :tags (seq-remove (lambda (x) (equal (alist-get 'id x) tag))
                                                        (plist-get l :tags))))))))

(defun scooter-kanban-tag-left ()
  "Move the tag at point one place earlier."
  (interactive)
  (scooter-kanban-edit--shift :tags 'scooter-kanban-tag-section "Tag: " -1))

(defun scooter-kanban-tag-right ()
  "Move the tag at point one place later."
  (interactive)
  (scooter-kanban-edit--shift :tags 'scooter-kanban-tag-section "Tag: " 1))

;;;; Text buffers

(defvar-local scooter-kanban-edit--on-save nil
  "Function of the buffer's text that saves it.")

(defvar-keymap scooter-kanban-text-mode-map
  :doc "Keymap for `scooter-kanban-text-mode'."
  "C-c C-c" #'scooter-kanban-text-save
  "C-c C-k" #'scooter-kanban-text-cancel)

(define-derived-mode scooter-kanban-text-mode text-mode "Scooter Text"
  "Edit a card's description or a skill; \\[scooter-kanban-text-save] saves."
  (setq-local header-line-format
              (substitute-command-keys
               "\\<scooter-kanban-text-mode-map>Markdown.  \\[scooter-kanban-text-save] saves, \\[scooter-kanban-text-cancel] cancels")))

(defun scooter-kanban-edit-text (name text on-save)
  "Edit TEXT in a buffer called NAME; ON-SAVE gets the text when saved.
An existing buffer called NAME is shown again rather than replaced."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (and scooter-kanban-edit--on-save (buffer-modified-p))
        (erase-buffer)
        (if (fboundp 'markdown-mode)
            (progn (funcall 'markdown-mode) (scooter-kanban-text-mode-keys))
          (scooter-kanban-text-mode))
        (insert (or text ""))
        (goto-char (point-min))
        (set-buffer-modified-p nil))
      (setq scooter-kanban-edit--on-save on-save))
    (pop-to-buffer buf)))

;;;; Files: front matter over a body

;; A skill is edited as the file the agent will see: `---' lines around
;; its fields, then the markdown body.

(defun scooter-kanban-edit-front-matter (fields)
  "Return FIELDS, an alist of (KEY . VALUE) strings, as front matter."
  (concat "---\n"
          (mapconcat (lambda (f) (format "%s: %s\n" (car f) (or (cdr f) ""))) fields "")
          "---\n\n"))

(defun scooter-kanban-edit-parse (text)
  "Split TEXT into (FIELDS . BODY).
FIELDS are the front matter's (KEY . VALUE) strings, nil when TEXT has
none; BODY is the rest, trimmed."
  (if (string-match "\\`---[ \t]*\n\\(\\(?:.*\n\\)*?\\)---[ \t]*\\(?:\n\\|\\'\\)" text)
      ;; Take both pieces before the per-line matches clobber the match data.
      (let ((head (match-string 1 text))
            (body (string-trim (substring text (match-end 0)))))
        (cons (delq nil (mapcar (lambda (line)
                                  (when (string-match "\\`\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*\\)\\'" line)
                                    (cons (match-string 1 line) (string-trim (match-string 2 line)))))
                                (split-string head "\n" t)))
              body))
    (cons nil (string-trim text))))

(defun scooter-kanban-edit-field (fields key &optional required)
  "Return KEY's value in FIELDS, \"\" when unset; signal when REQUIRED and empty."
  (let ((value (or (cdr (assoc key fields)) "")))
    (when (and required (string-empty-p value))
      (user-error "The front matter needs a %s" key))
    value))

(defun scooter-kanban-edit-true-p (value)
  "Non-nil when VALUE, a front matter string, says yes; unset is yes."
  (not (member (downcase (string-trim (or value ""))) '("false" "no" "off" "0" "nil"))))

(defun scooter-kanban-edit-file (name fields body on-save)
  "Edit FIELDS and BODY as one file in a buffer called NAME.
ON-SAVE gets the saved front matter, an alist of (KEY . VALUE) strings,
and the body."
  (scooter-kanban-edit-text
   name (concat (scooter-kanban-edit-front-matter fields) (or body ""))
   (lambda (text)
     (pcase-let ((`(,fields . ,body) (scooter-kanban-edit-parse text)))
       (unless fields (user-error "Keep the front matter: the lines between the two ---"))
       (funcall on-save fields body)))))

(defun scooter-kanban-text-mode-keys ()
  "Give a buffer in another major mode the text editor's keys."
  (use-local-map (make-composed-keymap scooter-kanban-text-mode-map (current-local-map)))
  (setq-local header-line-format
              (substitute-command-keys
               "\\<scooter-kanban-text-mode-map>\\[scooter-kanban-text-save] saves, \\[scooter-kanban-text-cancel] cancels")))

(defun scooter-kanban-text-save ()
  "Save this buffer's text and close it."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (on-save scooter-kanban-edit--on-save))
    (funcall on-save text)
    (quit-window t)))

(defun scooter-kanban-text-cancel ()
  "Close this buffer without saving."
  (interactive)
  (when (or (not (buffer-modified-p)) (yes-or-no-p "Discard your edits? "))
    (quit-window t)))

;;;###autoload
(defun scooter-kanban-edit-description ()
  "Edit the description of the card at point."
  (interactive)
  (pcase-let* ((`(,board . ,card) (scooter-kanban-at-point))
               (server (plist-get board :server))
               (id (scooter-kanban-card-id card))
               (details (scooter-kanban-card-request server id "GET" "details")))
    (scooter-kanban-edit-text
     (format "*scooter-card: %s*" (scooter-kanban-card-title card))
     (alist-get 'description details)
     (lambda (text)
       (scooter-kanban-card-request server id "PATCH" "details" `((description . ,text)))
       (scooter-kanban--after-change server id)
       (message "Saved the description")))))

;;;; Board skills

(defun scooter-kanban-edit--skill (board)
  "Return the skill id at point on BOARD, or read one."
  (scooter-kanban-edit--at 'scooter-kanban-skill-section board :skills "Skill: "))

(defun scooter-kanban-edit--board-skill-file (board skill)
  "Edit BOARD's SKILL as a file, or a new one when SKILL is nil."
  (let ((id (alist-get 'id skill)))
    (scooter-kanban-edit-file
     (format "*scooter-skill: %s (%s)*" (or (alist-get 'name skill) "new") (plist-get board :name))
     `(("name" . ,(or (alist-get 'name skill) "")))
     (alist-get 'body skill)
     (lambda (fields body)
       (let ((name (scooter-kanban-edit-field fields "name" t)))
         (when (string-empty-p body) (user-error "A skill needs a body"))
         (scooter-kanban-edit--save
          board (lambda (l)
                  (plist-put l :skills
                             (if id
                                 (scooter-kanban-edit--update
                                  (scooter-kanban-edit--update (plist-get l :skills) id 'name name)
                                  id 'body body)
                               (append (plist-get l :skills) (list `((name . ,name) (body . ,body))))))))
         (message "Saved the board skill %s" name))))))

(defun scooter-kanban-add-skill ()
  "Write a new skill for every agent on the board at point, in a buffer."
  (interactive)
  (scooter-kanban-edit--board-skill-file (scooter-kanban-board-at-point) nil))

(defun scooter-kanban-edit-skill ()
  "Edit the skill at point as a file: its name, then its text."
  (interactive)
  (let* ((board (scooter-kanban-board-at-point))
         (id (scooter-kanban-edit--skill board)))
    (scooter-kanban-edit--board-skill-file
     board (seq-find (lambda (k) (equal (alist-get 'id k) id)) (plist-get board :skills)))))

(defun scooter-kanban-rename-skill ()
  "Rename the skill at point."
  (interactive)
  (let* ((board (scooter-kanban-board-at-point))
         (id (scooter-kanban-edit--skill board))
         (old (alist-get 'name (seq-find (lambda (k) (equal (alist-get 'id k) id)) (plist-get board :skills))))
         (name (read-string "Rename skill: " old)))
    (scooter-kanban-edit--save
     board (lambda (l) (plist-put l :skills (scooter-kanban-edit--update (plist-get l :skills) id 'name name))))))

(defun scooter-kanban-delete-skill ()
  "Delete the skill at point."
  (interactive)
  (let* ((board (scooter-kanban-board-at-point))
         (id (scooter-kanban-edit--skill board)))
    (when (yes-or-no-p "Delete this skill? ")
      (scooter-kanban-edit--save
       board (lambda (l) (plist-put l :skills (seq-remove (lambda (k) (equal (alist-get 'id k) id))
                                                          (plist-get l :skills))))))))

;;;; Personal skills

(defun scooter-kanban-edit--user-skill-path (id)
  "Return the /user-skills route for the skill ID."
  (format "/user-skills/%s" (url-hexify-string id)))

(defun scooter-kanban-edit--user-skill-file (server skill)
  "Edit SERVER's personal SKILL as a file, or a new one when SKILL is nil.
Saving creates or updates it with the front matter's name, description
and enabled, and the body as its instructions."
  (let ((id (alist-get 'id skill)))
    (scooter-kanban-edit-file
     (format "*scooter-skill: %s@%s*" (or (alist-get 'name skill) "new") server)
     `(("name" . ,(or (alist-get 'name skill) ""))
       ("description" . ,(or (alist-get 'description skill) ""))
       ("enabled" . ,(if (or (null skill) (scooter-kanban-enabled-p skill)) "true" "false")))
     (alist-get 'body skill)
     (lambda (fields body)
       (let ((name (scooter-kanban-edit-field fields "name" t))
             (description (scooter-kanban-edit-field fields "description" t))
             (enabled (scooter-kanban-edit-true-p (scooter-kanban-edit-field fields "enabled"))))
         (when (string-empty-p body) (user-error "A skill needs instructions"))
         (let ((payload `((name . ,name) (description . ,description) (body . ,body)
                          (enabled . ,(if enabled t :json-false)))))
           (if id
               (scooter-request server "PATCH" (scooter-kanban-edit--user-skill-path id) payload)
             (scooter-request server "POST" "/user-skills" payload)))
         (scooter-kanban--after-change)
         (message "Saved the skill %s; it applies to conversations started from now on" name))))))

;;;###autoload
(defun scooter-kanban-new-user-skill (server)
  "Write a new personal skill on SERVER, in a buffer; C-c C-c saves it.
Its name, when to use it and whether it's enabled go in the front matter."
  (interactive (list (scooter-kanban-server-at-point)))
  (scooter-kanban-edit--user-skill-file server nil))

;;;###autoload
(defun scooter-kanban-edit-user-skill ()
  "Edit the personal skill at point as a file."
  (interactive)
  (pcase-let ((`(,server . ,skill) (or (scooter-kanban-user-skill-at-point)
                                      (scooter-kanban-edit--read-user-skill))))
    (scooter-kanban-edit--user-skill-file server skill)))

(defun scooter-kanban-edit--read-user-skill ()
  "Read one of the personal skills on the server at point; (SERVER . SKILL)."
  (let* ((server (scooter-kanban-server-at-point))
         (skills (scooter-kanban-user-skills server))
         (table (mapcar (lambda (k) (cons (alist-get 'name k) k)) skills)))
    (unless table (user-error "No personal skills on %s" server))
    (cons server (cdr (assoc (completing-read "Personal skill: " table nil t) table)))))

(defun scooter-kanban-edit--set-user-skill-enabled (server skill enabled)
  "Make SERVER's personal SKILL ENABLED or not."
  (scooter-request server "PATCH" (scooter-kanban-edit--user-skill-path (alist-get 'id skill))
                   `((enabled . ,(if enabled t :json-false))))
  (scooter-kanban--after-change)
  (message "%s the skill %s" (if enabled "Enabled" "Disabled") (alist-get 'name skill)))

;;;###autoload
(defun scooter-kanban-delete-user-skill ()
  "Delete the personal skill at point."
  (interactive)
  (pcase-let ((`(,server . ,skill) (or (scooter-kanban-user-skill-at-point)
                                      (scooter-kanban-edit--read-user-skill))))
    (when (yes-or-no-p (format "Delete the skill %s? " (alist-get 'name skill)))
      (scooter-request server "DELETE" (scooter-kanban-edit--user-skill-path (alist-get 'id skill)))
      (scooter-kanban--after-change))))

;;;; Hooks

(defun scooter-kanban-edit--set-hook-enabled (server hook enabled)
  "Turn SERVER's HOOK on or off, as ENABLED says."
  (unless (eq (alist-get 'userCanDisable hook) t)
    (user-error "The hook %s is required by the deployment" (alist-get 'id hook)))
  (scooter-request server "PATCH" (format "/user-hooks/%s" (url-hexify-string (alist-get 'id hook)))
                   `((enabled . ,(if enabled t :json-false))))
  (scooter-kanban--after-change)
  (message "%s the hook %s; it applies to conversations started from now on"
           (if enabled "Enabled" "Disabled") (alist-get 'id hook)))

;;;; At point

;;;###autoload
(defun scooter-kanban-edit-at-point ()
  "Edit what's at point: a personal or board skill as a file, or a card's text."
  (interactive)
  (cond
   ((scooter-kanban-user-skill-at-point) (scooter-kanban-edit-user-skill))
   ((scooter-kanban-section-of 'scooter-kanban-skill-section) (scooter-kanban-edit-skill))
   ((scooter-kanban-hook-at-point) (user-error "Hooks are the deployment's; ! turns this one on or off"))
   (t (scooter-kanban-edit-description))))

;;;###autoload
(defun scooter-kanban-toggle ()
  "Enable or disable the personal skill or hook at point."
  (interactive)
  (pcase-let ((`(,server . ,skill) (scooter-kanban-user-skill-at-point))
              (`(,hserver . ,hook) (scooter-kanban-hook-at-point)))
    (cond
     (skill (scooter-kanban-edit--set-user-skill-enabled server skill (not (scooter-kanban-enabled-p skill))))
     (hook (scooter-kanban-edit--set-hook-enabled hserver hook (not (scooter-kanban-enabled-p hook))))
     (t (user-error "No personal skill or hook here")))))

;;;###autoload (autoload 'scooter-kanban-edit-dispatch "scooter-kanban-edit" nil t)
(transient-define-prefix scooter-kanban-edit-dispatch ()
  "Edit the board at point: its stages, tags and skills; and your skills and hooks."
  [["Stage at point"
    ("a" "Add after" scooter-kanban-add-stage)
    ("r" "Rename" scooter-kanban-rename-stage)
    ("<" "Move left" scooter-kanban-stage-left)
    (">" "Move right" scooter-kanban-stage-right)
    ("d" "Delete" scooter-kanban-delete-stage)]
   ["Tag at point"
    ("t" "Add" scooter-kanban-add-tag)
    ("T" "Rename" scooter-kanban-rename-tag)
    ("c" "Color" scooter-kanban-color-tag)
    ("[" "Move left" scooter-kanban-tag-left)
    ("]" "Move right" scooter-kanban-tag-right)
    ("x" "Delete" scooter-kanban-delete-tag)]
   ["Board skill at point"
    ("s" "Add" scooter-kanban-add-skill)
    ("e" "Edit as a file" scooter-kanban-edit-skill)
    ("S" "Rename" scooter-kanban-rename-skill)
    ("k" "Delete" scooter-kanban-delete-skill)]
   ["Board"
    ("N" "New" scooter-kanban-new-board)
    ("R" "Rename" scooter-kanban-rename-board)
    ("D" "Delete" scooter-kanban-delete-board)]
   ["Your skills and hooks"
    ("i" "New personal skill" scooter-kanban-new-user-skill)
    ("u" "Edit personal skill" scooter-kanban-edit-user-skill)
    ("!" "Enable/disable at point" scooter-kanban-toggle)
    ("K" "Delete personal skill" scooter-kanban-delete-user-skill)]])

(provide 'scooter-kanban-edit)
;;; scooter-kanban-edit.el ends here
