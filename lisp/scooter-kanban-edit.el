;;; scooter-kanban-edit.el --- Edit Scooter's kanban boards  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Chad Crawford
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Commands that change a board itself: make, rename and delete boards;
;; add, rename, reorder and delete its stages and tags; write its skills,
;; the notes every agent on the board reads.  A board's stages, tags and
;; skills save together, so each command sends the whole layout as it
;; should end up, as the web UI's Edit board does.  Also the buffer that
;; edits a card's description or a skill's text.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
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
  "Edit a card's description or a board skill; \\[scooter-kanban-text-save] saves."
  (setq-local header-line-format
              (substitute-command-keys
               "\\<scooter-kanban-text-mode-map>Markdown.  \\[scooter-kanban-text-save] saves, \\[scooter-kanban-text-cancel] cancels")))

(defun scooter-kanban-edit-text (name text on-save)
  "Edit TEXT in a buffer called NAME; ON-SAVE gets the text when saved."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (if (fboundp 'markdown-mode)
          (progn (funcall 'markdown-mode) (scooter-kanban-text-mode-keys))
        (scooter-kanban-text-mode))
      (insert (or text ""))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq scooter-kanban-edit--on-save on-save))
    (pop-to-buffer buf)))

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

;;;; Skills

(defun scooter-kanban-edit--skill (board)
  "Return the skill id at point on BOARD, or read one."
  (scooter-kanban-edit--at 'scooter-kanban-skill-section board :skills "Skill: "))

(defun scooter-kanban-add-skill (name)
  "Write a new skill NAME for every agent on the board at point."
  (interactive (list (read-string "New skill's name: ")))
  (let ((board (scooter-kanban-board-at-point)))
    (when (string-empty-p (string-trim name)) (user-error "A skill needs a name"))
    (scooter-kanban-edit-text
     (format "*scooter-skill: %s*" name) ""
     (lambda (body)
       (scooter-kanban-edit--save
        board (lambda (l) (plist-put l :skills (append (plist-get l :skills)
                                                       (list `((name . ,name) (body . ,body)))))))))))

(defun scooter-kanban-edit-skill ()
  "Edit the text of the skill at point."
  (interactive)
  (let* ((board (scooter-kanban-board-at-point))
         (id (scooter-kanban-edit--skill board))
         (skill (seq-find (lambda (k) (equal (alist-get 'id k) id)) (plist-get board :skills))))
    (scooter-kanban-edit-text
     (format "*scooter-skill: %s*" (alist-get 'name skill)) (alist-get 'body skill)
     (lambda (body)
       (scooter-kanban-edit--save
        board (lambda (l) (plist-put l :skills (scooter-kanban-edit--update (plist-get l :skills) id 'body body))))))))

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

;;;###autoload (autoload 'scooter-kanban-edit-dispatch "scooter-kanban-edit" nil t)
(transient-define-prefix scooter-kanban-edit-dispatch ()
  "Edit the board at point: its stages, tags and skills."
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
   ["Skill at point"
    ("s" "Add" scooter-kanban-add-skill)
    ("e" "Edit text" scooter-kanban-edit-skill)
    ("S" "Rename" scooter-kanban-rename-skill)
    ("k" "Delete" scooter-kanban-delete-skill)]
   ["Board"
    ("N" "New" scooter-kanban-new-board)
    ("R" "Rename" scooter-kanban-rename-board)
    ("D" "Delete" scooter-kanban-delete-board)]])

(provide 'scooter-kanban-edit)
;;; scooter-kanban-edit.el ends here
