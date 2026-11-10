;;; scooter-kanban.el --- Scooter's kanban boards  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Chad Crawford
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `scooter-kanban' shows the kanban boards of every server as
;; `magit-section's: a board, its stages, and in each stage the cards of
;; the conversations filed there.  A card shows its status, title,
;; priority, tags, the agent's note, who moved it last and what blocks
;; it; TAB folds open its description, blockers and links.  RET opens the
;; card's conversation in agent-shell in another window, leaving the
;; board in view.
;;
;; A board is a locked folder only kanban changes, and each of its stages
;; a folder inside it, so a card's stage is the folder its conversation is
;; filed in.  Moving a card files the conversation in the stage's folder,
;; records the move with kanban, then tells the agent, as the web UI does.
;; Boards, stages and cards come from the kanban contrib through
;; /contrib-api/kanban, and refresh when /conversations/events reports a
;; change.
;;
;; After a server's boards come the user's own skills (/user-skills, the
;; settings page's Skills) and the deployment's hooks (/user-hooks), with
;; the commands in scooter-kanban-edit.el to write, toggle and delete them.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'parse-time)
(require 'magit-section)
(require 'transient)
(require 'scooter-server)
(require 'scooter-http)
(require 'scooter-list)
(require 'scooter-agent-shell)
(require 'scooter)

(declare-function scooter-sandbox "scooter-conversation")
(declare-function scooter-web-services "scooter-conversation")
(declare-function scooter-links "scooter-conversation")
(autoload 'scooter-kanban-edit-dispatch "scooter-kanban-edit" nil t)
(autoload 'scooter-kanban-edit-description "scooter-kanban-edit" nil t)
(autoload 'scooter-kanban-edit-at-point "scooter-kanban-edit" nil t)
(autoload 'scooter-kanban-toggle "scooter-kanban-edit" nil t)
(autoload 'scooter-kanban-new-user-skill "scooter-kanban-edit" nil t)
(autoload 'scooter-kanban-edit-user-skill "scooter-kanban-edit" nil t)
(autoload 'scooter-kanban-delete-user-skill "scooter-kanban-edit" nil t)
(autoload 'scooter-kanban-new-board "scooter-kanban-edit" nil t)
(autoload 'scooter-kanban-rename-board "scooter-kanban-edit" nil t)
(autoload 'scooter-kanban-delete-board "scooter-kanban-edit" nil t)
(declare-function evil-set-initial-state "evil-core" (mode state))
(defvar agent-shell-display-action)
(declare-function agent-shell-subscribe-to "agent-shell")

(defcustom scooter-kanban-lane-limit 50
  "Cards a stage shows at first, the most recently active; `+' shows more."
  :type 'natnum
  :group 'scooter)

(defcustom scooter-kanban-fold-last-stage t
  "Non-nil folds each board's last stage, where finished cards pile up."
  :type 'boolean
  :group 'scooter)

(defcustom scooter-kanban-announce-moves t
  "Non-nil tells a card's agent when you move it, as the web UI does.
The message starts a run, so the agent can act on the new stage."
  :type 'boolean
  :group 'scooter)

(defcustom scooter-kanban-display-action
  '((display-buffer-reuse-window display-buffer-use-some-window display-buffer-pop-up-window)
    (inhibit-same-window . t))
  "How a card's conversation is shown: in another window, the board kept in view."
  :type '(cons (repeat function) alist)
  :group 'scooter)

(defcustom scooter-kanban-open-in 'window
  "Where opening a card shows its conversation.
`window': in another window, the board kept in view.
`tab': in a tab-bar tab of its own, named after the card.  Opening the
card again switches to its tab, and closing the tab closes the
conversation's shell; the conversation itself stays on the server.
`C-c C-k' in a shell then goes to the board's tab."
  :type '(choice (const :tag "Another window" window)
                 (const :tag "A tab of its own" tab))
  :group 'scooter)

(defface scooter-kanban-board '((t :inherit (magit-section-heading bold) :height 1.1))
  "Face for board names."
  :group 'scooter)

(defface scooter-kanban-stage '((t :inherit magit-section-secondary-heading))
  "Face for stage names."
  :group 'scooter)

(defface scooter-kanban-note '((t :inherit default))
  "Face for the agent's note on a card."
  :group 'scooter)

(defface scooter-kanban-blocked '((t :inherit (warning bold)))
  "Face for a card's \"Blocked by\" mark."
  :group 'scooter)

(defface scooter-kanban-urgent '((t :inherit (error bold)))
  "Face for the Urgent priority mark."
  :group 'scooter)

(defface scooter-kanban-high '((t :inherit (warning bold)))
  "Face for the High priority mark."
  :group 'scooter)

(defconst scooter-kanban-tag-colors
  '(("gray" . "gray55") ("red" . "#e5484d") ("orange" . "#f76b15") ("yellow" . "#d6a400")
    ("green" . "#30a46c") ("teal" . "#12a594") ("blue" . "#3e83f8") ("purple" . "#8e4ec6"))
  "The web UI's tag palette, by the color names kanban stores.")

(defconst scooter-kanban-priorities '("none" "low" "medium" "high" "urgent")
  "Card priorities, lowest first.")

(defconst scooter-kanban-max-read 200
  "Cards kanban reads per request.")

;;;; Requests

(defun scooter-kanban--path (path)
  "Return kanban's user API PATH."
  (concat "/contrib-api/kanban/" path))

(defun scooter-kanban-request (server method path &optional body)
  "Send METHOD to kanban's user route PATH on SERVER with BODY."
  (scooter-request server method (scooter-kanban--path path) body))

(defun scooter-kanban-card-request (server id method path &optional body)
  "Send METHOD to kanban's route PATH for SERVER's conversation ID, with BODY."
  (scooter-request server method
                   (format "/conversations/%s/contrib-api/kanban/%s" (url-hexify-string id) path)
                   body))

(defun scooter-kanban--board-path (board &optional tail)
  "Return BOARD's user API path, followed by TAIL."
  (concat "boards/" (url-hexify-string board) tail))

;;;; Model

;; A board is a plist:
;;   :server :id :name :stages :tags :skills :cards
;; with :stages, :tags and :skills as kanban sends them, and :cards a list of
;; card plists:
;;   :conv (the /conversations alist) :stage (a stage id) :card :blockers
;;   :tags (tag ids) :priority

(defvar-local scooter-kanban--servers nil
  "Servers this buffer shows.")

(defvar-local scooter-kanban--data nil
  "The last rendered list of (SERVER . BOARDS-OR-ERROR).")

(defvar-local scooter-kanban--user nil
  "Each server's personal skills and hooks, as (SERVER . PLIST).
PLIST has :skills, the /user-skills reply, and :hooks, the /user-hooks
reply; either is a string when the request failed, or nil when the
server has no such route.")

(defvar-local scooter-kanban--limits nil
  "Stage id to the number of cards it shows, once `+' raised it.")

(defvar-local scooter-kanban--streams nil
  "Live event stream processes, one per server.")

(defvar-local scooter-kanban--refresh-timer nil
  "Pending debounced refresh.")

(defvar-local scooter-kanban--stale nil
  "Non-nil when a change arrived while the buffer was out of sight.")

(defun scooter-kanban--key (id)
  "Return ID as the symbol key kanban's JSON objects parse to."
  (intern id))

(defun scooter-kanban--stage-of (placement stages)
  "Return the stage id of PLACEMENT among STAGES; the first for the board root."
  (if (seq-find (lambda (s) (equal (alist-get 'id s) placement)) stages)
      placement
    (alist-get 'id (car stages))))

(defun scooter-kanban--read-cards (server board ids)
  "Return kanban's view of conversations IDS on SERVER's BOARD, keyed by id symbol."
  (let (cards)
    (while ids
      (let ((batch (seq-take ids scooter-kanban-max-read)))
        (setq ids (seq-drop ids scooter-kanban-max-read)
              cards (append cards
                            (alist-get 'cards
                                       (scooter-kanban-request
                                        server "POST" (scooter-kanban--board-path board "/cards")
                                        `((conversations . ,(vconcat batch)))))))))
    cards))

(defun scooter-kanban--boards (server)
  "Return SERVER's boards with their cards, as plists."
  (let* ((rows (alist-get 'boards (scooter-kanban-request server "GET" "boards")))
         (folders (scooter-request server "GET" "/folders"))
         (names (mapcar (lambda (f) (cons (alist-get 'id f) (alist-get 'name f)))
                        (alist-get 'folders folders)))
         (placements (alist-get 'placements folders))
         (convs (seq-remove (lambda (c) (alist-get 'parentId c))
                            (scooter-request server "GET" "/conversations"))))
    (delq nil
          (mapcar
           (lambda (row)
             (let* ((id (alist-get 'folder row))
                    (stages (alist-get 'stages row))
                    (homes (cons id (mapcar (lambda (s) (alist-get 'id s)) stages)))
                    (mine (seq-filter (lambda (c)
                                        (member (alist-get (scooter-kanban--key (alist-get 'id c)) placements)
                                                homes))
                                      convs))
                    (views (and mine stages
                                (scooter-kanban--read-cards server id (mapcar (lambda (c) (alist-get 'id c)) mine)))))
               (when (and (assoc id names) stages)
                 (list :server server :id id :name (cdr (assoc id names))
                       :stages stages :tags (alist-get 'tags row) :skills (alist-get 'skills row)
                       :cards (mapcar
                               (lambda (c)
                                 (let* ((key (scooter-kanban--key (alist-get 'id c)))
                                        (view (alist-get key views)))
                                   (list :conv c
                                         :stage (scooter-kanban--stage-of (alist-get key placements) stages)
                                         :card (alist-get 'card view)
                                         :blockers (alist-get 'blockers view)
                                         :tags (alist-get 'tags view)
                                         :priority (alist-get 'priority view))))
                               mine)))))
           rows))))

(defun scooter-kanban--fetch (server)
  "Return SERVER's boards, or an error string."
  (condition-case err
      (scooter-kanban--boards server)
    (scooter-http-error
     (if (string-match-p ": 404 " (error-message-string err))
         "kanban is not enabled on this server"
       (error-message-string err)))
    (error (error-message-string err))))

;;;; Personal skills and hooks

;; The server's settings page keeps personal skills (name, when to use
;; it, markdown body, enabled) at /user-skills and the deployment's
;; hooks, with each user's opt-outs, at /user-hooks.  Both are per user,
;; not per board, so they show under the server.

(defun scooter-kanban--user-get (server path)
  "GET PATH on SERVER: the reply, nil when SERVER lacks it, else an error string."
  (condition-case err
      (scooter-request server "GET" path)
    (scooter-http-error
     (let ((msg (error-message-string err)))
       (cond ((string-match-p ": 404 " msg) nil)
             ((string-match-p ": 501 " msg) "not configured on this server")
             (t msg))))
    (error (error-message-string err))))

(defun scooter-kanban--fetch-user (server)
  "Return SERVER's personal skills and hooks as a plist."
  (list :skills (scooter-kanban--user-get server "/user-skills")
        :hooks (scooter-kanban--user-get server "/user-hooks")))

(defun scooter-kanban-user-skills (server)
  "Return SERVER's personal skills as the kanban buffer last read them."
  (let ((reply (plist-get (alist-get server scooter-kanban--user) :skills)))
    (and (listp reply) (alist-get 'skills reply))))

(defun scooter-kanban-enabled-p (item)
  "Non-nil when ITEM, a skill or hook alist, is enabled."
  (eq (alist-get 'enabled item) t))

(defun scooter-kanban-card-id (card)
  "Return CARD's conversation id."
  (alist-get 'id (plist-get card :conv)))

(defun scooter-kanban-card-title (card)
  "Return CARD's title, or its short id when untitled."
  (let ((title (alist-get 'title (plist-get card :conv))))
    (if (and title (not (string-empty-p title))) title
      (scooter-slug nil (scooter-kanban-card-id card)))))

(defun scooter-kanban-stage-name (board stage)
  "Return the name of BOARD's STAGE id."
  (or (alist-get 'label (seq-find (lambda (s) (equal (alist-get 'id s) stage)) (plist-get board :stages)))
      stage))

(defun scooter-kanban--position (card)
  "Return CARD's position in its stage."
  (or (alist-get 'position (plist-get card :card)) 0))

(defun scooter-kanban-lane (board stage)
  "Return BOARD's cards in STAGE, in board order."
  (cl-sort (seq-filter (lambda (c) (equal (plist-get c :stage) stage)) (plist-get board :cards))
           (lambda (a b)
             (let ((pa (scooter-kanban--position a)) (pb (scooter-kanban--position b)))
               (or (< pa pb)
                   (and (= pa pb)
                        (> (or (alist-get 'lastActivityAt (plist-get a :conv)) 0)
                           (or (alist-get 'lastActivityAt (plist-get b :conv)) 0))))))))

(defun scooter-kanban--page (lane limit)
  "Return LANE's LIMIT most recently active cards, in LANE's order."
  (if (<= (length lane) limit) lane
    (let ((keep (seq-take (cl-sort (copy-sequence lane) #'>
                                   :key (lambda (c) (or (alist-get 'lastActivityAt (plist-get c :conv)) 0)))
                          limit)))
      (seq-filter (lambda (c) (memq c keep)) lane))))

(defun scooter-kanban-waiting-on (board card)
  "Return the blockers CARD on BOARD still waits on.
A blocker is done once in the board's last stage, wherever it was read."
  (let ((last (alist-get 'id (car (last (plist-get board :stages))))))
    (seq-remove (lambda (b)
                  (if-let* ((on (seq-find (lambda (c) (equal (scooter-kanban-card-id c) (alist-get 'id b)))
                                          (plist-get board :cards))))
                      (equal (plist-get on :stage) last)
                    (eq (alist-get 'done b) t)))
                (plist-get card :blockers))))

(defun scooter-kanban-card-tags (board card)
  "Return BOARD's tags that CARD has."
  (let ((ids (plist-get card :tags)))
    (seq-filter (lambda (tag) (member (alist-get 'id tag) ids)) (plist-get board :tags))))

;;;; Rendering

(defclass scooter-kanban-server-section (magit-section) ())
(defclass scooter-kanban-board-section (magit-section) ())
(defclass scooter-kanban-stage-section (magit-section) ())
(defclass scooter-kanban-more-section (magit-section) ())
(defclass scooter-kanban-tag-section (magit-section) ())
(defclass scooter-kanban-skill-section (magit-section) ())
(defclass scooter-kanban-settings-section (magit-section) ())
(defclass scooter-kanban-user-section (magit-section) ())

;; A personal skill or a hook; the value is its id, so point finds it
;; again after a redraw, and the row holds what the server sent.
(defclass scooter-kanban-user-skill-section (magit-section)
  ((server :initarg :server :initform nil)
   (skill :initarg :skill :initform nil)))
(defclass scooter-kanban-hook-section (magit-section)
  ((server :initarg :server :initform nil)
   (hook :initarg :hook :initform nil)))

;; A card is a `scooter-conversation-section', so the list's commands
;; (star, retitle, stop, sandbox, links, ...) act on it too.
(defclass scooter-kanban-card-section (scooter-conversation-section)
  ((board :initarg :board :initform nil)
   (card :initarg :card :initform nil)))

(cl-defmethod magit-section-ident-value ((section scooter-kanban-board-section))
  "Identify SECTION by its board id."
  (plist-get (oref section value) :id))

(defun scooter-kanban--age (iso)
  "Format ISO, an ISO 8601 time, as the web UI's \"5m ago\"."
  (if-let* ((time (ignore-errors (float-time (parse-iso8601-time-string iso)))))
      (let ((m (round (- (float-time) time) 60)))
        (cond ((< m 1) "just now")
              ((< m 60) (format "%dm ago" m))
              ((< m (* 48 60)) (format "%dh ago" (round m 60)))
              (t (format "%dd ago" (round m (* 24 60))))))
    ""))

(defun scooter-kanban--updated (card)
  "Who last moved CARD, and when."
  (if-let* ((row (plist-get card :card)))
      (format "%s · %s" (if (equal (alist-get 'updated_by row) "agent") "Agent" "You")
              (scooter-kanban--age (alist-get 'updated_at row)))
    "No status yet"))

(defun scooter-kanban-tag-string (tag)
  "Return TAG as a colored chip."
  (propertize (alist-get 'name tag)
              'font-lock-face `(:foreground ,(or (cdr (assoc (alist-get 'color tag) scooter-kanban-tag-colors))
                                                 "gray55")
                                :weight bold)))

(defun scooter-kanban--priority-mark (priority)
  "Return the mark for PRIORITY: High and Urgent only, as on the web board."
  (pcase priority
    ("urgent" (propertize "URGENT" 'font-lock-face 'scooter-kanban-urgent))
    ("high" (propertize "HIGH" 'font-lock-face 'scooter-kanban-high))))

(defun scooter-kanban--heading (board card)
  "Return CARD's heading lines on BOARD."
  (let* ((conv (plist-get card :conv))
         (waiting (scooter-kanban-waiting-on board card))
         (note (alist-get 'note (plist-get card :card)))
         (first (string-join
                 (delq nil (list (scooter-list--col-status conv)
                                 (propertize (scooter-kanban-card-title card) 'font-lock-face 'bold)
                                 (scooter-kanban--priority-mark (plist-get card :priority))
                                 (when-let* ((tags (scooter-kanban-card-tags board card)))
                                   (string-join (mapcar #'scooter-kanban-tag-string tags) " "))))
                 " "))
         (second (string-join
                  (delq nil (list (and note (not (string-empty-p note))
                                       (propertize note 'font-lock-face 'scooter-kanban-note))
                                  (propertize (scooter-kanban--updated card) 'font-lock-face 'scooter-list-dim)
                                  (and waiting
                                       (propertize (format "Blocked by %d" (length waiting))
                                                   'font-lock-face 'scooter-kanban-blocked
                                                   'help-echo (mapconcat (lambda (b) (alist-get 'title b))
                                                                         waiting "\n")))))
                  " · ")))
    (concat first "\n      " second)))

(defun scooter-kanban--related-line (label related)
  "Insert a details line LABEL listing RELATED cards."
  (when related
    (scooter-list--insert-detail
     label (mapconcat (lambda (b)
                        (format "%s (%s%s)" (alist-get 'title b) (alist-get 'stage b)
                                (if (eq (alist-get 'done b) t) ", done" "")))
                      related "; "))))

(defun scooter-kanban--insert-details (board card)
  "Insert CARD's details on BOARD, read when its row first folds open."
  (let* ((server (plist-get board :server))
         (id (scooter-kanban-card-id card))
         (details (condition-case err
                      (scooter-kanban-card-request server id "GET" "details")
                    (error (insert "      " (propertize (error-message-string err) 'font-lock-face 'error) "\n")
                           nil))))
    (when details
      (scooter-list--insert-detail "stage" (scooter-kanban-stage-name board (plist-get card :stage)))
      (scooter-list--insert-detail "priority" (let ((p (alist-get 'priority details)))
                                                (unless (equal p "none") p)))
      (scooter-kanban--related-line "blocked by" (alist-get 'blocked_by details))
      (scooter-kanban--related-line "blocking" (alist-get 'blocking details))
      (let ((desc (string-trim (or (alist-get 'description details) ""))))
        (unless (string-empty-p desc)
          ;; The label is wider than the details' label column: a line of its own.
          (insert (make-string 5 ?\s) (propertize "description" 'font-lock-face 'scooter-list-dim) "\n")
          (dolist (line (split-string desc "\n"))
            (insert (make-string 7 ?\s) line "\n")))))
    (scooter-list--insert-details server (plist-get card :conv))))

(defun scooter-kanban--insert-card (board card)
  "Insert CARD on BOARD, its details folded beneath."
  (magit-insert-section (scooter-kanban-card-section (cons (plist-get board :server) (plist-get card :conv)) t
                                                     :board board :card card)
    (magit-insert-heading (concat "    " (scooter-kanban--heading board card)))
    (magit-insert-section-body
      (scooter-kanban--insert-details board card))))

(defun scooter-kanban--insert-stage (board stage last)
  "Insert BOARD's STAGE, folded when LAST and `scooter-kanban-fold-last-stage'."
  (let* ((id (alist-get 'id stage))
         (lane (scooter-kanban-lane board id))
         (limit (or (alist-get id scooter-kanban--limits nil nil #'equal) scooter-kanban-lane-limit))
         (shown (scooter-kanban--page lane limit)))
    (magit-insert-section (scooter-kanban-stage-section id (and last scooter-kanban-fold-last-stage))
      (magit-insert-heading
        (concat "  " (propertize (alist-get 'label stage) 'font-lock-face 'scooter-kanban-stage)
                (propertize (format " (%d)" (length lane)) 'font-lock-face 'scooter-list-dim)))
      (dolist (card shown) (scooter-kanban--insert-card board card))
      (when (< (length shown) (length lane))
        (magit-insert-section (scooter-kanban-more-section id)
          (insert (propertize (format "    + %d more (RET)\n" (- (length lane) (length shown)))
                              'font-lock-face 'scooter-list-dim)))))))

(defun scooter-kanban--insert-settings (board)
  "Insert BOARD's tags and skills, folded."
  (magit-insert-section (scooter-kanban-settings-section (plist-get board :id) t)
    (magit-insert-heading
      (propertize (format "  Tags and skills (%d, %d)"
                          (length (plist-get board :tags)) (length (plist-get board :skills)))
                  'font-lock-face 'scooter-list-dim))
    (dolist (tag (plist-get board :tags))
      (magit-insert-section (scooter-kanban-tag-section (alist-get 'id tag))
        (insert "    tag    " (scooter-kanban-tag-string tag)
                (propertize (format "  %s" (alist-get 'color tag)) 'font-lock-face 'scooter-list-dim) "\n")))
    (dolist (skill (plist-get board :skills))
      (magit-insert-section (scooter-kanban-skill-section (alist-get 'id skill))
        (insert "    skill  " (alist-get 'name skill)
                (propertize (format "  board-%s" (scooter-slug (alist-get 'name skill)))
                            'font-lock-face 'scooter-list-dim)
                "\n")))
    (insert (propertize "    E edits stages, tags and skills\n" 'font-lock-face 'scooter-list-dim))))

(defun scooter-kanban--insert-board (board)
  "Insert BOARD: its stages and their cards."
  (let ((stages (plist-get board :stages)))
    (magit-insert-section (scooter-kanban-board-section board)
      (magit-insert-heading
        (propertize (plist-get board :name) 'font-lock-face 'scooter-kanban-board)
        (propertize (format "  %d card%s" (length (plist-get board :cards))
                            (if (= (length (plist-get board :cards)) 1) "" "s"))
                    'font-lock-face 'scooter-list-dim))
      (cl-loop for (stage . more) on stages
               do (scooter-kanban--insert-stage board stage (null more)))
      (scooter-kanban--insert-settings board)
      (insert "\n"))))

(defun scooter-kanban--on-off (on)
  "Return the mark for something ON or off."
  (if on (propertize "●" 'font-lock-face 'scooter-list-running)
    (propertize "○" 'font-lock-face 'scooter-list-idle)))

(defun scooter-kanban--insert-user-skill (server skill builtin)
  "Insert SERVER's personal SKILL; BUILTIN names the deployment's skills."
  (let ((name (alist-get 'name skill))
        (description (string-trim (or (alist-get 'description skill) ""))))
    (magit-insert-section (scooter-kanban-user-skill-section (alist-get 'id skill) nil
                                                              :server server :skill skill)
      (magit-insert-heading
        (concat "  " (scooter-kanban--on-off (scooter-kanban-enabled-p skill)) " "
                (propertize name 'font-lock-face 'bold)
                (if (scooter-kanban-enabled-p skill) ""
                  (propertize "  disabled" 'font-lock-face 'scooter-list-dim))
                (if (member name builtin)
                    (propertize "  replaces a built-in" 'font-lock-face 'scooter-kanban-high)
                  "")
                (if (string-empty-p description) ""
                  (concat "\n    " (propertize (car (split-string description "\n"))
                                               'font-lock-face 'scooter-list-dim))))))))

(defun scooter-kanban--insert-hook (server hook)
  "Insert SERVER's HOOK."
  (let ((description (string-trim (or (alist-get 'description hook) ""))))
    (magit-insert-section (scooter-kanban-hook-section (alist-get 'id hook) nil
                                                       :server server :hook hook)
      (magit-insert-heading
        (concat "  " (scooter-kanban--on-off (scooter-kanban-enabled-p hook)) " "
                (propertize (alist-get 'id hook) 'font-lock-face 'bold)
                (propertize (format "  %s" (alist-get 'event hook)) 'font-lock-face 'scooter-list-model)
                (cond ((not (eq (alist-get 'userCanDisable hook) t))
                       (propertize "  required" 'font-lock-face 'scooter-list-dim))
                      ((scooter-kanban-enabled-p hook) "")
                      (t (propertize "  off" 'font-lock-face 'scooter-list-dim)))
                (if (string-empty-p description) ""
                  (concat "\n    " (propertize (car (split-string description "\n"))
                                               'font-lock-face 'scooter-list-dim))))))))

(defun scooter-kanban--insert-user (server user)
  "Insert SERVER's personal skills and hooks from USER, its fetched plist.
A server without the routes shows neither section."
  (let ((skills (plist-get user :skills))
        (hooks (plist-get user :hooks)))
    (when skills
      (magit-insert-section (scooter-kanban-user-section (cons 'skills server))
        (magit-insert-heading
          (propertize "Skills" 'font-lock-face 'scooter-kanban-board)
          (propertize (if (stringp skills) "" (format "  %d" (length (alist-get 'skills skills))))
                      'font-lock-face 'scooter-list-dim))
        (cond
         ((stringp skills)
          (insert "  " (propertize (concat "Personal skills: " skills) 'font-lock-face 'error) "\n"))
         ((null (alist-get 'skills skills))
          (insert (propertize "  No personal skills yet; i writes one.\n" 'font-lock-face 'scooter-list-dim)))
         (t (dolist (skill (alist-get 'skills skills))
              (scooter-kanban--insert-user-skill server skill (alist-get 'builtinNames skills)))))
        (insert "\n")))
    (when hooks
      (magit-insert-section (scooter-kanban-user-section (cons 'hooks server) t)
        (magit-insert-heading
          (propertize "Hooks" 'font-lock-face 'scooter-kanban-board)
          (propertize (if (stringp hooks) "" (format "  %d" (length (alist-get 'hooks hooks))))
                      'font-lock-face 'scooter-list-dim))
        (cond
         ((stringp hooks)
          (insert "  " (propertize (concat "Hooks: " hooks) 'font-lock-face 'error) "\n"))
         ((null (alist-get 'hooks hooks))
          (insert (propertize "  The deployment has no hooks.\n" 'font-lock-face 'scooter-list-dim)))
         (t (dolist (hook (alist-get 'hooks hooks))
              (scooter-kanban--insert-hook server hook))
            (unless (eq (alist-get 'configurable hooks) t)
              (insert (propertize "  Hooks can't be turned off on this server.\n"
                                  'font-lock-face 'scooter-list-dim)))))
        (insert "\n")))))

(defun scooter-kanban--render (data &optional user)
  "Render DATA, a list of (SERVER . BOARDS-OR-ERROR).
USER is each server's personal skills and hooks, as (SERVER . PLIST)."
  (let ((inhibit-read-only t)
        (pos (point))
        (here (scooter-kanban--point-ident)))
    (setq scooter-kanban--data data)
    (setq scooter-kanban--user user)
    (erase-buffer)
    (magit-insert-section (root)
      (pcase-dolist (`(,server . ,boards) data)
        (magit-insert-section (scooter-kanban-server-section server)
          (magit-insert-heading
            (propertize (format "%s" server) 'font-lock-face 'scooter-list-server)
            (propertize (format "  %s" (scooter-server-url server)) 'font-lock-face 'scooter-list-dim)
            (propertize (if (stringp boards) "" (format "  %d board%s" (length boards)
                                                        (if (= (length boards) 1) "" "s")))
                        'font-lock-face 'scooter-list-dim))
          ;; Each board ends with a blank line; the other two cases need one added.
          (cond
           ((stringp boards) (insert "  " (propertize boards 'font-lock-face 'error) "\n\n"))
           ((null boards) (insert (propertize "  No boards yet; N makes one.\n\n" 'font-lock-face 'scooter-list-dim)))
           (t (mapc #'scooter-kanban--insert-board boards)))
          (scooter-kanban--insert-user server (alist-get server user)))))
    (unless (and here (scooter-kanban--goto-ident here))
      (goto-char (min pos (point-max))))))

(defun scooter-kanban--point-ident ()
  "Return the identity of the section at point, to find it after a redraw."
  (when-let* ((s (magit-current-section)))
    (magit-section-ident s)))

(defun scooter-kanban--goto-ident (ident)
  "Move to the section with IDENT; non-nil when found."
  (when-let* ((s (magit-get-section ident)))
    (goto-char (oref s start))))

;;;; Buffer

(defvar-keymap scooter-kanban-mode-map
  :doc "Keymap for `scooter-kanban-mode'."
  :parent magit-section-mode-map
  "RET" #'scooter-kanban-visit
  "o" #'scooter-kanban-display
  "C-o" #'scooter-kanban-display
  "]" #'scooter-kanban-next-card
  "[" #'scooter-kanban-previous-card
  ">" #'scooter-kanban-move-right
  "<" #'scooter-kanban-move-left
  "m" #'scooter-kanban-move
  "M-<up>" #'scooter-kanban-move-up
  "M-<down>" #'scooter-kanban-move-down
  "c" #'scooter-kanban-new
  "a" #'scooter-kanban-add
  "u" #'scooter-kanban-remove
  "#" #'scooter-kanban-set-tags
  "P" #'scooter-kanban-set-priority
  "e" #'scooter-kanban-edit-at-point
  "i" #'scooter-kanban-new-user-skill
  "!" #'scooter-kanban-toggle
  "x" #'scooter-kanban-add-blocker
  "X" #'scooter-kanban-remove-blocker
  "+" #'scooter-kanban-more
  "N" #'scooter-kanban-new-board
  "R" #'scooter-kanban-rename-board
  "D" #'scooter-kanban-delete-board
  "E" #'scooter-kanban-edit-dispatch
  "s" #'scooter-list-star
  "t" #'scooter-list-retitle
  "k" #'scooter-list-cancel
  "z" #'scooter-list-suspend
  "f" #'scooter-list-files
  "b" #'scooter-list-browse
  "w" #'scooter-list-copy-url
  "v" #'scooter-web-services
  "l" #'scooter-links
  "C-c C-b" #'scooter-sandbox
  "g" #'scooter-kanban-refresh
  "?" #'scooter-kanban-dispatch)

(define-derived-mode scooter-kanban-mode magit-section-mode "Scooter Kanban"
  "Scooter's kanban boards: each stage's cards, one per conversation."
  (setq-local revert-buffer-function (lambda (&rest _) (scooter-kanban-refresh)))
  (add-hook 'window-buffer-change-functions #'scooter-kanban--shown nil t)
  (add-hook 'kill-buffer-hook #'scooter-kanban--stop-streams nil t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'scooter-kanban-mode 'emacs))

(transient-define-prefix scooter-kanban-dispatch ()
  "Commands for the kanban boards."
  [["Card at point"
    ("RET" "Open chat" scooter-kanban-visit)
    ("o" "Show chat" scooter-kanban-display)
    ("]" "Next card's chat" scooter-kanban-next-card)
    ("[" "Previous card's chat" scooter-kanban-previous-card)
    ("e" "Edit description, or skill at point" scooter-kanban-edit-at-point)
    ("P" "Priority" scooter-kanban-set-priority)
    ("#" "Tags" scooter-kanban-set-tags)
    ("x" "Add blocker" scooter-kanban-add-blocker)
    ("X" "Remove blocker" scooter-kanban-remove-blocker)]
   ["Move"
    (">" "Next stage" scooter-kanban-move-right)
    ("<" "Previous stage" scooter-kanban-move-left)
    ("m" "To stage…" scooter-kanban-move)
    ("M-<up>" "Up" scooter-kanban-move-up)
    ("M-<down>" "Down" scooter-kanban-move-down)
    ("u" "Off the board" scooter-kanban-remove)]
   ["Conversation"
    ("s" "Star" scooter-list-star)
    ("t" "Retitle" scooter-list-retitle)
    ("k" "Stop run" scooter-list-cancel)
    ("z" "Suspend" scooter-list-suspend)
    ("C-c C-b" "Sandbox…" scooter-sandbox)
    ("v" "Web services" scooter-web-services)
    ("l" "Links" scooter-links)
    ("b" "Browse" scooter-list-browse)]
   ["Board"
    ("c" "New conversation here" scooter-kanban-new)
    ("a" "Add a conversation" scooter-kanban-add)
    ("E" "Stages, tags, skills…" scooter-kanban-edit-dispatch)
    ("N" "New board" scooter-kanban-new-board)
    ("R" "Rename board" scooter-kanban-rename-board)
    ("D" "Delete board" scooter-kanban-delete-board)
    ("g" "Refresh" scooter-kanban-refresh)
    ("S" "More Scooter commands" scooter-dispatch)]
   ["Skills and hooks"
    ("i" "New personal skill" scooter-kanban-new-user-skill)
    ("!" "Enable/disable at point" scooter-kanban-toggle)]
   ["Fold"
    ("TAB" "This one" magit-section-toggle)
    ("M-2" "To boards" magit-section-show-level-2-all)
    ("M-3" "To stages" magit-section-show-level-3-all)
    ("M-4" "To cards" magit-section-show-level-4-all)]])

(defun scooter-kanban--buffer ()
  "Return the kanban buffer, showing every server's boards."
  (let ((buf (get-buffer-create "*scooter-kanban*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'scooter-kanban-mode)
        (scooter-kanban-mode)
        (setq scooter-kanban--servers (seq-remove (lambda (s) (scooter-server-get s :self))
                                                  (scooter-server-names)))
        (scooter-kanban-refresh)
        (scooter-kanban--start-streams)))
    buf))

;;;###autoload
(defun scooter-kanban (&optional conversation)
  "Show the kanban boards of every Scooter server.
From a Scooter shell, show them beside it with point on its card;
CONVERSATION is (SERVER ID) to go to instead."
  (interactive)
  (let* ((conversation (or conversation (scooter-agent-shell-conversation)))
         (buf (scooter-kanban--buffer)))
    (cond
     ((and (eq scooter-kanban-open-in 'tab) (or conversation (scooter-kanban--tab-number buf)))
      (scooter-kanban--show-in-tab buf "Kanban"))
     (conversation (pop-to-buffer buf scooter-kanban-display-action))
     (t (pop-to-buffer-same-window buf)))
    (when (and conversation
               (not (with-current-buffer buf
                      (scooter-kanban--goto-card (car conversation) (cadr conversation)))))
      (message "This conversation is on no board; `a' on a stage adds it"))))

(defun scooter-kanban--goto-card (server id)
  "Move to SERVER's card ID, unfolding its board and stage; non-nil when found."
  (let (found)
    (magit-map-sections
     (lambda (s)
       (when (and (not found) (cl-typep s 'scooter-kanban-card-section)
                  (eq (car (oref s value)) server)
                  (equal (alist-get 'id (cdr (oref s value))) id))
         (setq found s)))
     magit-root-section)
    (when found
      (let ((p (oref found parent)))
        (while p
          (magit-section-show p)
          (setq p (oref p parent))))
      (goto-char (oref found start))
      found)))

(defun scooter-kanban-refresh ()
  "Refetch and redraw the boards."
  (interactive)
  (setq scooter-kanban--stale nil)
  (scooter-kanban--render
   (mapcar (lambda (s) (cons s (scooter-kanban--fetch s))) scooter-kanban--servers)
   (mapcar (lambda (s) (cons s (scooter-kanban--fetch-user s))) scooter-kanban--servers)))

(defun scooter-kanban--visible-p (buf)
  "Non-nil when BUF shows in some window."
  (get-buffer-window buf 'visible))

(defun scooter-kanban--shown (win)
  "Refresh WIN's board if a change arrived while it was hidden."
  (with-current-buffer (window-buffer win)
    (when scooter-kanban--stale
      (ignore-errors (scooter-kanban-refresh)))))

(defun scooter-kanban--schedule-refresh (buf)
  "Refresh BUF soon, coalescing bursts; later if it is out of sight."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (unless (timerp scooter-kanban--refresh-timer)
        (setq scooter-kanban--refresh-timer
              (run-at-time 1 nil
                           (lambda ()
                             (when (buffer-live-p buf)
                               (with-current-buffer buf
                                 (setq scooter-kanban--refresh-timer nil)
                                 (if (scooter-kanban--visible-p buf)
                                     (ignore-errors (save-excursion (scooter-kanban-refresh)))
                                   (setq scooter-kanban--stale t)))))))))))

(defun scooter-kanban--start-streams ()
  "Follow each shown server's change stream."
  (scooter-kanban--stop-streams)
  (dolist (server scooter-kanban--servers)
    (scooter-kanban--follow (current-buffer) server)))

(defun scooter-kanban--follow (buf server)
  "Follow SERVER's change stream for BUF, reconnecting while BUF lives."
  (when (buffer-live-p buf)
    (when-let* ((proc (ignore-errors
                        (scooter-http-stream server "/conversations/events"
                                             (lambda (frame)
                                               (unless (equal (alist-get 'kind frame) "snapshot")
                                                 (scooter-kanban--schedule-refresh buf)))))))
      (set-process-sentinel
       proc (lambda (_p _e)
              (when (buffer-live-p buf)
                (run-at-time 10 nil #'scooter-kanban--follow buf server))))
      (with-current-buffer buf (push proc scooter-kanban--streams)))))

(defun scooter-kanban--stop-streams ()
  "Stop this buffer's change streams."
  (dolist (p scooter-kanban--streams)
    (set-process-sentinel p #'ignore)
    (delete-process p))
  (setq scooter-kanban--streams nil))

;;;; Point

(defun scooter-kanban--card-section ()
  "Return the card section at point, or nil."
  (let ((s (magit-current-section)))
    (while (and s (not (cl-typep s 'scooter-kanban-card-section)))
      (setq s (oref s parent)))
    s))

(defun scooter-kanban-at-point ()
  "Return (BOARD . CARD) at point, or signal."
  (if-let* ((s (scooter-kanban--card-section)))
      (cons (oref s board) (oref s card))
    (user-error "No card here")))

(defun scooter-kanban-board-at-point (&optional noerror)
  "Return the board whose section holds point; signal unless NOERROR."
  (let ((s (magit-current-section)))
    (while (and s (not (cl-typep s 'scooter-kanban-board-section)))
      (setq s (oref s parent)))
    (cond (s (oref s value))
          (noerror nil)
          (t (user-error "No board here")))))

(defun scooter-kanban-stage-at-point (board)
  "Return the stage id at point on BOARD, or read one."
  (let ((s (magit-current-section)))
    (while (and s (not (cl-typep s 'scooter-kanban-stage-section)))
      (setq s (oref s parent)))
    (if s (oref s value) (scooter-kanban-read-stage board "Stage: "))))

(defun scooter-kanban-server-at-point ()
  "Return the server whose section holds point."
  (let ((s (magit-current-section)))
    (while (and s (not (cl-typep s 'scooter-kanban-server-section)))
      (setq s (oref s parent)))
    (if s (oref s value) (scooter-current-server))))

(defun scooter-kanban-section-of (class)
  "Return the CLASS section holding point, or nil."
  (let ((s (magit-current-section)))
    (while (and s (not (cl-typep s class)))
      (setq s (oref s parent)))
    s))

(defun scooter-kanban-user-skill-at-point ()
  "Return (SERVER . SKILL) for the personal skill at point, or nil."
  (when-let* ((s (scooter-kanban-section-of 'scooter-kanban-user-skill-section)))
    (cons (oref s server) (oref s skill))))

(defun scooter-kanban-hook-at-point ()
  "Return (SERVER . HOOK) for the hook at point, or nil."
  (when-let* ((s (scooter-kanban-section-of 'scooter-kanban-hook-section)))
    (cons (oref s server) (oref s hook))))

(defun scooter-kanban-read-stage (board prompt &optional exclude)
  "Read one of BOARD's stages with PROMPT, other than EXCLUDE; return its id."
  (let* ((table (seq-remove (lambda (c) (equal (cdr c) exclude))
                            (mapcar (lambda (s) (cons (alist-get 'label s) (alist-get 'id s)))
                                    (plist-get board :stages)))))
    (cdr (assoc (completing-read prompt table nil t) table))))

;;;; Opening cards

(defun scooter-kanban--shell (server id)
  "Return a live agent-shell buffer on SERVER's conversation ID, or nil."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (and (derived-mode-p 'agent-shell-mode)
                     (equal (ignore-errors (scooter-agent-shell-conversation)) (list server id)))))
            (buffer-list)))

(defun scooter-kanban--show-in-window (server id &optional select)
  "Show SERVER's conversation ID in another window, selecting it when SELECT.
A shell already open on it is reused."
  (require 'agent-shell)
  (let ((board (selected-window))
        (buf (scooter-kanban--shell server id)))
    (if buf
        (display-buffer buf scooter-kanban-display-action)
      (let ((agent-shell-display-action scooter-kanban-display-action))
        (setq buf (scooter-open server id)))
      ;; agent-shell may show the shell only once its session is picked,
      ;; after this binding is gone; showing it now, it just selects it then.
      (when (buffer-live-p buf)
        (unless (get-buffer-window buf)
          (when (window-live-p board) (select-window board))
          (display-buffer buf scooter-kanban-display-action))
        (unless select
          (agent-shell-subscribe-to
           :shell-buffer buf :event 'session-selected
           :on-event (lambda (_)
                       (run-at-time 0 nil (lambda ()
                                            (when (window-live-p board)
                                              (select-window board)))))))))
    (if (and select (buffer-live-p buf) (get-buffer-window buf))
        (select-window (get-buffer-window buf))
      (when (window-live-p board) (select-window board)))))

;;;; Opening cards in tabs

(defvar-local scooter-kanban--tabbed nil
  "Non-nil in a shell `scooter-kanban' opened in a tab of its own.")

(defvar scooter-kanban--closing nil
  "Non-nil while a tab and its shell are being closed together.")

(defun scooter-kanban--tab-number (buf)
  "Return the number of a tab other than the current one showing BUF, or nil."
  (require 'tab-bar)
  (when-let* ((tab (tab-bar-get-buffer-tab buf nil t)))
    (1+ (alist-get 'index tab))))

(defun scooter-kanban--tab-buffers (tab)
  "Return the buffers TAB's windows show."
  (if (eq (car tab) 'current-tab)
      (delete-dups (mapcar #'window-buffer (window-list nil 'no-minibuf)))
    (when-let* ((state (alist-get 'ws tab)))
      (delete-dups (delq nil (mapcar #'get-buffer (window-state-buffers state)))))))

(defun scooter-kanban--show-in-tab (buf name)
  "Select a tab showing BUF, or make one named NAME showing only BUF.
Return BUF's window there."
  (require 'tab-bar)
  (if-let* ((n (scooter-kanban--tab-number buf)))
      (tab-bar-select-tab n)
    (unless (and (eq (window-buffer) buf) (one-window-p t))
      (tab-bar-new-tab)
      (tab-bar-rename-tab name)
      (switch-to-buffer buf nil t)
      (delete-other-windows)))
  (let ((win (get-buffer-window buf)))
    (when win (select-window win))
    win))

(defun scooter-kanban--shell-buffer-p (buf)
  "Non-nil when BUF is an agent-shell."
  (with-current-buffer buf (derived-mode-p 'agent-shell-mode)))

(defun scooter-kanban--tab-shell (buf)
  "Make BUF's tab and shell close together."
  (with-current-buffer buf
    (setq scooter-kanban--tabbed t)
    (add-hook 'kill-buffer-hook #'scooter-kanban--close-tab nil t))
  (add-hook 'tab-bar-tab-pre-close-functions #'scooter-kanban--kill-tab-shells))

(defun scooter-kanban--kill-tab-shells (tab _only)
  "Kill the shells TAB was opened for, unless another tab shows them."
  (unless scooter-kanban--closing
    (let ((scooter-kanban--closing t))
      (dolist (buf (scooter-kanban--tab-buffers tab))
        (when (and (buffer-live-p buf)
                   (buffer-local-value 'scooter-kanban--tabbed buf)
                   (<= (length (tab-bar-get-buffer-tab buf nil nil t)) 1))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))))))

(defun scooter-kanban--close-tab ()
  "Close the tabs that show only this shell, which is being killed."
  (unless scooter-kanban--closing
    (let ((scooter-kanban--closing t)
          (buf (current-buffer)))
      (dolist (tab (tab-bar-get-buffer-tab buf nil nil t))
        (when (and (cdr (funcall tab-bar-tabs-function))
                   (equal (scooter-kanban--tab-buffers tab) (list buf)))
          (tab-bar-close-tab (1+ (alist-get 'index tab))))))))

(defun scooter-kanban--show-tabbed (server id name select)
  "Show SERVER's conversation ID in a tab named NAME, selecting it when SELECT."
  (require 'agent-shell)
  (require 'tab-bar)
  (let ((buf (scooter-kanban--shell server id))
        (fresh nil))
    (unless buf
      ;; agent-shell may show the shell once its session is picked, after
      ;; this returns: it keeps this override for then, so the shell still
      ;; goes to its tab rather than over the board.
      (let ((display-buffer-overriding-action
             `((,(lambda (b _alist)
                   (when (scooter-kanban--shell-buffer-p b)
                     (scooter-kanban--show-in-tab b name)))))))
        (setq buf (scooter-open server id)
              fresh t)))
    (when (buffer-live-p buf)
      (scooter-kanban--show-in-tab buf name)
      (scooter-kanban--tab-shell buf)
      (unless select
        (tab-bar-switch-to-recent-tab)
        (when fresh
          (agent-shell-subscribe-to
           :shell-buffer buf :event 'session-selected
           :on-event (lambda (_)
                       (run-at-time 0 nil (lambda ()
                                            (when (and (buffer-live-p buf) (get-buffer-window buf))
                                              (tab-bar-switch-to-recent-tab)))))))))))

(defun scooter-kanban-show-conversation (server id &optional select name)
  "Show SERVER's conversation ID, selecting it when SELECT.
It goes in another window, or a tab named NAME, as
`scooter-kanban-open-in' says."
  (if (eq scooter-kanban-open-in 'tab)
      (scooter-kanban--show-tabbed server id (or name id) select)
    (scooter-kanban--show-in-window server id select)))

(defun scooter-kanban--card-title (card)
  "CARD's title, to name its tab."
  (or (alist-get 'title (plist-get card :conv)) (scooter-kanban-card-id card)))

(defun scooter-kanban-visit ()
  "Open the card at point's conversation and go there, in a window or tab.
On a \"more\" line, show more of the stage; elsewhere fold the section."
  (interactive)
  (let ((s (magit-current-section)))
    (cond
     ((cl-typep s 'scooter-kanban-more-section) (scooter-kanban-more))
     ((scooter-kanban-user-skill-at-point) (scooter-kanban-edit-user-skill))
     ((scooter-kanban-hook-at-point) (scooter-kanban-toggle))
     ((scooter-kanban--card-section)
      (let ((card (cdr (scooter-kanban-at-point))))
        (scooter-kanban-show-conversation (plist-get (car (scooter-kanban-at-point)) :server)
                                          (scooter-kanban-card-id card) t
                                          (scooter-kanban--card-title card))))
     (s (magit-section-toggle s)))))

(defun scooter-kanban-display ()
  "Show the card at point's conversation, staying here.
It shows in another window, or opens in a tab in the background, as
`scooter-kanban-open-in' says."
  (interactive)
  (pcase-let ((`(,board . ,card) (scooter-kanban-at-point)))
    (scooter-kanban-show-conversation (plist-get board :server) (scooter-kanban-card-id card)
                                      nil (scooter-kanban--card-title card))))

(defun scooter-kanban--step (by)
  "Move BY cards in board order and show that card's conversation.
It always shows in another window, even with `scooter-kanban-open-in'
set to `tab': stepping through cards shouldn't open a tab for each one."
  (let ((cards nil) (here (scooter-kanban--card-section)))
    (magit-map-sections (lambda (s) (when (cl-typep s 'scooter-kanban-card-section) (push s cards)))
                        magit-root-section)
    (setq cards (nreverse cards))
    (let* ((at (if here (seq-position cards here) (if (> by 0) -1 (length cards))))
           (next (and at (nth (+ at by) cards))))
      (unless (and next (>= (+ at by) 0)) (user-error "No more cards"))
      (goto-char (oref next start))
      (pcase-let ((`(,board . ,card) (scooter-kanban-at-point)))
        (scooter-kanban--show-in-window (plist-get board :server) (scooter-kanban-card-id card))))))

(defun scooter-kanban-next-card ()
  "Show the next card's conversation."
  (interactive)
  (scooter-kanban--step 1))

(defun scooter-kanban-previous-card ()
  "Show the previous card's conversation."
  (interactive)
  (scooter-kanban--step -1))

(defun scooter-kanban-more ()
  "Show more cards of the stage at point."
  (interactive)
  (let* ((board (scooter-kanban-board-at-point))
         (stage (scooter-kanban-stage-at-point board)))
    (setf (alist-get stage scooter-kanban--limits nil nil #'equal)
          (+ (or (alist-get stage scooter-kanban--limits nil nil #'equal) scooter-kanban-lane-limit)
             scooter-kanban-lane-limit))
    (save-excursion (scooter-kanban--render scooter-kanban--data scooter-kanban--user))))

;;;; Moving cards

(defun scooter-kanban--between (before after)
  "Return a position strictly between BEFORE and AFTER; either may be nil."
  (cond ((and before after) (/ (+ before after) 2.0))
        (before (1+ before))
        (after (1- after))
        (t 0)))

(defun scooter-kanban--put-card (server id stage position)
  "Record SERVER's card ID in STAGE at POSITION."
  (scooter-kanban-card-request server id "PUT" "card" `((column . ,stage) (position . ,position))))

(defun scooter-kanban-place (board card stage index)
  "Move CARD on BOARD into STAGE before the card at INDEX of its lane.
A move to another stage files the conversation in the stage's folder
first and, with `scooter-kanban-announce-moves', tells its agent."
  (let* ((server (plist-get board :server))
         (id (scooter-kanban-card-id card))
         (from (plist-get card :stage))
         (lane (seq-remove (lambda (c) (equal (scooter-kanban-card-id c) id))
                           (scooter-kanban-lane board stage)))
         (index (max 0 (min index (length lane))))
         (positions (mapcar #'scooter-kanban--position lane))
         (tied (cl-loop for (a b) on positions while b thereis (<= b a))))
    (unless (equal from stage)
      (scooter-request server "PUT" (format "/conversations/%s/folder" (url-hexify-string id))
                       `((folderId . ,stage))))
    (if tied
        ;; Tied positions can't hold a card between them: renumber the lane.
        (cl-loop for c in (append (seq-take lane index) (list card) (seq-drop lane index))
                 for i from 0
                 unless (and (not (eq c card)) (= (scooter-kanban--position c) i))
                 do (scooter-kanban--put-card server (scooter-kanban-card-id c) stage i))
      (scooter-kanban--put-card server id stage
                                (scooter-kanban--between (and (> index 0) (nth (1- index) positions))
                                                         (nth index positions))))
    (when (and scooter-kanban-announce-moves (not (equal from stage)))
      (scooter-request server "POST" (format "/conversations/%s/messages" (url-hexify-string id))
                       `((text . ,(format "I moved this card from **%s** to **%s** on the \"%s\" board."
                                          (scooter-kanban-stage-name board from)
                                          (scooter-kanban-stage-name board stage)
                                          (plist-get board :name))))))
    (scooter-kanban--after-change server id)))

(defun scooter-kanban--after-change (&optional server id)
  "Redraw the kanban buffer, keeping point on SERVER's card ID if given."
  (when-let* ((buf (get-buffer "*scooter-kanban*")))
    (with-current-buffer buf
      (scooter-kanban-refresh)
      (when id (scooter-kanban--goto-card server id)))))

(defun scooter-kanban--shift-stage (by)
  "Move the card at point BY stages along its board."
  (pcase-let* ((`(,board . ,card) (scooter-kanban-at-point))
               (ids (mapcar (lambda (s) (alist-get 'id s)) (plist-get board :stages)))
               (to (nth (+ by (seq-position ids (plist-get card :stage))) ids)))
    (unless (and to (>= (+ by (seq-position ids (plist-get card :stage))) 0))
      (user-error "No stage that way"))
    (scooter-kanban-place board card to (length (scooter-kanban-lane board to)))))

(defun scooter-kanban-move-right ()
  "Move the card at point to the next stage."
  (interactive)
  (scooter-kanban--shift-stage 1))

(defun scooter-kanban-move-left ()
  "Move the card at point to the previous stage."
  (interactive)
  (scooter-kanban--shift-stage -1))

(defun scooter-kanban-move (stage)
  "Move the card at point to STAGE, read from its board's stages."
  (interactive (pcase-let ((`(,board . ,card) (scooter-kanban-at-point)))
                 (list (scooter-kanban-read-stage board "Move to stage: " (plist-get card :stage)))))
  (pcase-let ((`(,board . ,card) (scooter-kanban-at-point)))
    (scooter-kanban-place board card stage (length (scooter-kanban-lane board stage)))))

(defun scooter-kanban--shift-lane (by)
  "Move the card at point BY places within its stage."
  (pcase-let* ((`(,board . ,card) (scooter-kanban-at-point))
               (stage (plist-get card :stage))
               (at (seq-position (scooter-kanban-lane board stage) card #'eq))
               (to (+ at by)))
    (when (or (< to 0) (>= to (length (scooter-kanban-lane board stage))))
      (user-error "Already at the %s" (if (< by 0) "top" "bottom")))
    (scooter-kanban-place board card stage to)))

(defun scooter-kanban-move-up ()
  "Move the card at point up its stage."
  (interactive)
  (scooter-kanban--shift-lane -1))

(defun scooter-kanban-move-down ()
  "Move the card at point down its stage."
  (interactive)
  (scooter-kanban--shift-lane 1))

;;;; Filing conversations

(defun scooter-kanban--read-board (server)
  "Read one of SERVER's boards from the kanban buffer's data."
  (let* ((boards (cdr (assq server (with-current-buffer (scooter-kanban--buffer) scooter-kanban--data))))
         (table (if (consp boards)
                    (mapcar (lambda (b) (cons (plist-get b :name) b)) boards)
                  (user-error "No boards on %s" server))))
    (cdr (assoc (completing-read "Board: " table nil t) table))))

(defun scooter-kanban--file (board stage id)
  "File BOARD's SERVER conversation ID in STAGE, at the end of its lane."
  (let ((server (plist-get board :server)))
    (scooter-request server "PUT" (format "/conversations/%s/folder" (url-hexify-string id))
                     `((folderId . ,stage)))
    (scooter-kanban--put-card server id stage
                              (scooter-kanban--between
                               (car (last (mapcar #'scooter-kanban--position (scooter-kanban-lane board stage))))
                               nil))))

(defun scooter-kanban-new (text)
  "Start a conversation with TEXT as a card in the stage at point, and open it."
  (interactive (list (read-string "New card's first message: ")))
  (let* ((board (scooter-kanban-board-at-point))
         (stage (scooter-kanban-stage-at-point board))
         (server (plist-get board :server))
         (id (alist-get 'id (scooter-request server "POST" "/conversations"
                                             (and (scooter-server-get server :model)
                                                  `((model . ,(scooter-server-get server :model))))))))
    (scooter-kanban--file board stage id)
    (scooter-request server "POST" (format "/conversations/%s/messages" (url-hexify-string id))
                     `((text . ,text)))
    (scooter-kanban--after-change server id)
    (scooter-kanban-show-conversation server id t (truncate-string-to-width text 30 nil nil "…"))))

(defun scooter-kanban-add (id)
  "Put the server's conversation ID on the board at point, in the stage at point."
  (interactive (list (let* ((board (scooter-kanban-board-at-point))
                            (on (mapcar #'scooter-kanban-card-id (plist-get board :cards)))
                            (convs (seq-remove (lambda (c) (or (member (alist-get 'id c) on)
                                                               (alist-get 'parentId c)))
                                               (scooter-request (plist-get board :server) "GET" "/conversations")))
                            (table (mapcar (lambda (c) (cons (format "%s  %s" (scooter-list--col-title c)
                                                                     (scooter-list--age (alist-get 'lastActivityAt c)))
                                                             (alist-get 'id c)))
                                           convs)))
                       (cdr (assoc (completing-read "Add conversation: " table nil t) table)))))
  (let* ((board (scooter-kanban-board-at-point))
         (stage (scooter-kanban-stage-at-point board)))
    (scooter-kanban--file board stage id)
    (scooter-kanban--after-change (plist-get board :server) id)))

(defun scooter-kanban-remove ()
  "Take the card at point off its board, leaving its conversation unfiled."
  (interactive)
  (pcase-let* ((`(,board . ,card) (scooter-kanban-at-point)))
    (when (yes-or-no-p (format "Take %s off %s? " (scooter-kanban-card-title card) (plist-get board :name)))
      (scooter-request (plist-get board :server) "PUT"
                       (format "/conversations/%s/folder" (url-hexify-string (scooter-kanban-card-id card)))
                       '((folderId . nil)))
      (scooter-kanban--after-change))))

;;;###autoload
(defun scooter-kanban-file (server id)
  "Put SERVER's conversation ID on a board, in a stage you pick.
In a Scooter shell, file its conversation; elsewhere pick one."
  (interactive (or (scooter-agent-shell-conversation)
                   (let ((server (scooter-current-server current-prefix-arg)))
                     (list server (scooter-read-conversation server)))))
  (let* ((board (scooter-kanban--read-board server))
         (card (seq-find (lambda (c) (equal (scooter-kanban-card-id c) id)) (plist-get board :cards)))
         (stage (scooter-kanban-read-stage board "Stage: ")))
    (if card
        (scooter-kanban-place board card stage (length (scooter-kanban-lane board stage)))
      (scooter-kanban--file board stage id)
      (scooter-kanban--after-change server id))
    (message "Filed in %s / %s" (plist-get board :name) (scooter-kanban-stage-name board stage))))

;;;; Card details

(defun scooter-kanban-set-priority (priority)
  "Set the card at point's PRIORITY."
  (interactive (list (completing-read "Priority: " scooter-kanban-priorities nil t nil nil
                                      (plist-get (cdr (scooter-kanban-at-point)) :priority))))
  (pcase-let ((`(,board . ,card) (scooter-kanban-at-point)))
    (scooter-kanban-card-request (plist-get board :server) (scooter-kanban-card-id card)
                                 "PATCH" "details" `((priority . ,priority)))
    (scooter-kanban--after-change (plist-get board :server) (scooter-kanban-card-id card))))

(defun scooter-kanban-set-tags (names)
  "Replace the card at point's tags with NAMES; new names become board tags."
  (interactive (pcase-let ((`(,board . ,card) (scooter-kanban-at-point)))
                 (list (completing-read-multiple
                        "Tags (comma-separated; new names make tags): "
                        (mapcar (lambda (tag) (alist-get 'name tag)) (plist-get board :tags))
                        nil nil
                        (mapconcat (lambda (tag) (alist-get 'name tag))
                                   (scooter-kanban-card-tags board card) ",")))))
  (pcase-let* ((`(,board . ,card) (scooter-kanban-at-point))
               (names (delete-dups (seq-remove #'string-empty-p (mapcar #'string-trim names))))
               (known (mapcar (lambda (tag) (cons (downcase (alist-get 'name tag)) (alist-get 'id tag)))
                              (plist-get board :tags)))
               (ids (delq nil (mapcar (lambda (n) (cdr (assoc (downcase n) known))) names)))
               (create (seq-remove (lambda (n) (assoc (downcase n) known)) names)))
    (when (> (length names) 5) (user-error "A card takes at most 5 tags"))
    (scooter-kanban-card-request (plist-get board :server) (scooter-kanban-card-id card)
                                 "PUT" "card/tags"
                                 `((tags . ,(vconcat ids)) (board . ,(plist-get board :id))
                                   (create . ,(vconcat create))))
    (scooter-kanban--after-change (plist-get board :server) (scooter-kanban-card-id card))))

(defun scooter-kanban--blocker-ids (board card)
  "Return the ids of the cards CARD on BOARD waits on, done or not."
  (mapcar (lambda (b) (alist-get 'id b))
          (alist-get 'blocked_by (scooter-kanban-card-request (plist-get board :server)
                                                              (scooter-kanban-card-id card) "GET" "details"))))

(defun scooter-kanban--set-blockers (board card ids)
  "Make CARD on BOARD wait on the cards IDS."
  (scooter-kanban-card-request (plist-get board :server) (scooter-kanban-card-id card)
                               "PUT" "card/blockers" `((blocked_by . ,(vconcat ids))))
  (scooter-kanban--after-change (plist-get board :server) (scooter-kanban-card-id card)))

(defun scooter-kanban-add-blocker ()
  "Make the card at point wait on another card of its board."
  (interactive)
  (pcase-let* ((`(,board . ,card) (scooter-kanban-at-point))
               (have (scooter-kanban--blocker-ids board card))
               (table (mapcar (lambda (c) (cons (format "%s  [%s]" (scooter-kanban-card-title c)
                                                        (scooter-kanban-stage-name board (plist-get c :stage)))
                                                (scooter-kanban-card-id c)))
                              (seq-remove (lambda (c) (member (scooter-kanban-card-id c)
                                                              (cons (scooter-kanban-card-id card) have)))
                                          (plist-get board :cards)))))
    (unless table (user-error "No other card to wait on"))
    (scooter-kanban--set-blockers
     board card (append have (list (cdr (assoc (completing-read "Blocked by: " table nil t) table)))))))

(defun scooter-kanban-remove-blocker ()
  "Stop the card at point waiting on one of its blockers."
  (interactive)
  (pcase-let* ((`(,board . ,card) (scooter-kanban-at-point))
               (details (scooter-kanban-card-request (plist-get board :server)
                                                     (scooter-kanban-card-id card) "GET" "details"))
               (table (or (mapcar (lambda (b) (cons (alist-get 'title b) (alist-get 'id b)))
                                  (alist-get 'blocked_by details))
                          (user-error "Nothing blocks this card")))
               (drop (cdr (assoc (completing-read "Unblock from: " table nil t) table))))
    (scooter-kanban--set-blockers board card (delete drop (mapcar #'cdr table)))))

(provide 'scooter-kanban)
;;; scooter-kanban.el ends here
