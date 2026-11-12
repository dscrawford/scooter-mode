;;; scooter-kanban-test.el --- Tests for the kanban boards  -*- lexical-binding: t; -*-

(require 'ert)
(require 'scooter)
(require 'scooter-kanban)
(require 'scooter-kanban-edit)
(require 'scooter-conversation)

(setq auth-sources nil)

(defvar scooter-kanban-test--calls nil
  "Requests the fake server got, newest first, as (METHOD PATH BODY).")

(defun scooter-kanban-test--iso (minutes-ago)
  "An ISO 8601 time MINUTES-AGO minutes back."
  (format-time-string "%FT%T+00:00" (time-subtract nil (* 60 minutes-ago)) t))

(defun scooter-kanban-test--reply (method path _body)
  "The fake server's reply to METHOD PATH."
  (pcase (list method path)
    (`("GET" "/contrib-api/kanban/boards")
     `((boards . (((folder . "b1")
                   (stages . (((id . "s1") (label . "Design") (position . 0))
                              ((id . "s2") (label . "In progress") (position . 1))
                              ((id . "s3") (label . "Done") (position . 2))))
                   (tags . (((id . "t1") (name . "dotfiles") (color . "blue") (position . 0))))
                   (skills . (((id . "k1") (name . "Repos") (body . "Use the fork.") (position . 0)))))
                  ;; Its folder is gone from the sidebar, so it isn't shown.
                  ((folder . "gone") (stages . (((id . "g1") (label . "X") (position . 0))))
                   (tags) (skills))))))
    (`("GET" "/folders")
     '((folders . (((id . "b1") (name . "Work") (kind . "user"))
                   ((id . "s1") (name . "Design") (kind . "user") (parentId . "b1"))))
       (placements . ((c1 . "s1") (c2 . "s1") (c3 . "s2") (c4 . "b1") (c5 . "s3") (c6 . "elsewhere")))))
    (`("GET" "/conversations")
     `(((id . "c1") (title . "Fix login") (runStartedAt . 1) (lastActivityAt . 5))
       ((id . "c2") (title . "Write docs") (lastActivityAt . 4))
       ((id . "c3") (title . "Kanban in Emacs") (awaitingInputAt . 1) (lastActivityAt . 3))
       ((id . "c4") (title . "Root filed") (lastActivityAt . 2))
       ((id . "c5") (title . "Shipped") (lastActivityAt . 1))
       ((id . "c6") (title . "Unfiled") (lastActivityAt . 9))
       ((id . "c7") (title . "Subagent") (parentId . "c1"))))
    (`("POST" "/contrib-api/kanban/boards/b1/cards")
     `((cards . ((c1 . ((card . ((column . "s1") (position . 2) (note . "PR #4 open, CI green")
                                 (updated_by . "agent") (updated_at . ,(scooter-kanban-test--iso 5))))
                        (blockers . (((id . "c3") (title . "Kanban in Emacs") (stage . "In progress") (done . :false))))
                        (tags . ("t1")) (priority . "high")))
                 (c2 . ((card . ((column . "s1") (position . 1) (note . "")
                                 (updated_by . "user") (updated_at . ,(scooter-kanban-test--iso 120))))
                        (blockers . (((id . "c5") (title . "Shipped") (stage . "Done") (done . t))))
                        (tags) (priority . "none")))
                 (c3 . ((card . nil) (blockers) (tags) (priority . "urgent")))))))
    (`("GET" ,(rx "/conversations/" (+ alnum) "/contrib-api/kanban/details"))
     '((description . "Make the board work.\nIn Emacs.") (priority . "high")
       (blocked_by . (((id . "c3") (title . "Kanban in Emacs") (stage . "In progress") (done . :false))))
       (blocking)))
    (`("GET" "/user-skills")
     '((skills . (((id . "u1") (name . "acme-conventions") (description . "Use in acme/app.\nAnd its PRs.")
                   (body . "Run `just ci` before a PR.") (enabled . t))
                  ((id . "u2") (name . "github-workflow") (description . "Old notes.")
                   (body . "Push to main.") (enabled . :false))))
       (builtinNames . ("github-workflow" "kanban"))))
    (`("GET" "/user-hooks")
     '((configurable . t)
       (hooks . (((id . "finish-or-ask") (event . "Stop") (description . "Back to work instead of ending.")
                  (userCanDisable . :false) (enabled . t))
                 ((id . "lint") (event . "PostToolUse") (description . "Lint after edits.")
                  (userCanDisable . t) (enabled . t))))))
    (_ nil)))

(defmacro scooter-kanban-test-with-board (&rest body)
  "Run BODY in a kanban buffer on the fake server, requests in `scooter-kanban-test--calls'."
  (declare (indent 0))
  `(let ((scooter-servers '((home :url "https://s.example")))
         (scooter-kanban-test--calls nil)
         (scooter-kanban-fold-last-stage nil))
     (cl-letf (((symbol-function 'scooter-request)
                (lambda (_server method path &optional body)
                  (push (list method path body) scooter-kanban-test--calls)
                  (scooter-kanban-test--reply method path body)))
               ((symbol-function 'scooter-kanban--start-streams) #'ignore))
       (when (get-buffer "*scooter-kanban*") (kill-buffer "*scooter-kanban*"))
       (save-window-excursion
         (scooter-kanban)
         (unwind-protect (with-current-buffer "*scooter-kanban*" ,@body)
           (kill-buffer "*scooter-kanban*"))))))

(defun scooter-kanban-test--goto (text)
  "Put point at the start of the line holding TEXT."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(defun scooter-kanban-test--writes ()
  "The non-GET requests, oldest first, minus the refresh's card read."
  (seq-remove (lambda (c) (or (equal (car c) "GET") (string-suffix-p "/cards" (cadr c))))
              (reverse scooter-kanban-test--calls)))

(ert-deftest scooter-kanban-test-renders-stages-and-cards ()
  (scooter-kanban-test-with-board
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "^home  https://s.example  1 board$" text))
      (should (string-match-p "^Work  5 cards$" text))
      (should-not (string-match-p "gone\\|Unfiled\\|Subagent" text))
      ;; Board order is by position; the board root's card, with no row yet,
      ;; sits at 0 in the first stage.
      (should (string-match-p
               "  Design (3)\n    .*Root filed\n.*No status yet\n    .*Write docs\n.*You · 2h ago\n    ● Fix login HIGH dotfiles\n.*PR #4 open, CI green · Agent · 5m ago · Blocked by 1\n"
               text))
      (should (string-match-p "  In progress (1)\n    ! Kanban in Emacs URGENT\n" text))
      (should (string-match-p "  Done (1)\n" text))
      (should (string-match-p "tag    dotfiles  blue" text))
      (should (string-match-p "skill  Repos  board-repos" text)))))

(ert-deftest scooter-kanban-test-done-blockers-dont-block ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Write docs")
    (pcase-let ((`(,board . ,card) (scooter-kanban-at-point)))
      (should (equal (plist-get card :stage) "s1"))
      (should-not (scooter-kanban-waiting-on board card)))))

(ert-deftest scooter-kanban-test-details-fold-open ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Fix login")
    (magit-section-show (magit-current-section))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "blocked by +Kanban in Emacs (In progress)" text))
      (should (string-match-p "     description\n       Make the board work.\n       In Emacs.\n" text))
      (should (string-match-p "url +https://s.example/chat/c1" text)))
    ;; A display stretch where the folded text starts would still be drawn,
    ;; indenting the next stage's heading once the card folds closed.
    (let ((body (oref (magit-current-section) content)))
      (should-not (get-text-property body 'display)))))

(ert-deftest scooter-kanban-test-move-right-files-records-and-tells ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Fix login")
    (scooter-kanban-move-right)
    (should (equal (scooter-kanban-test--writes)
                   '(("PUT" "/conversations/c1/folder" ((folderId . "s2")))
                     ("PUT" "/conversations/c1/contrib-api/kanban/card" ((column . "s2") (position . 1)))
                     ("POST" "/conversations/c1/messages"
                      ((text . "I moved this card from **Design** to **In progress** on the \"Work\" board."))))))
    ;; Point stays on the moved card after the redraw.
    (should (equal (scooter-kanban-card-id (cdr (scooter-kanban-at-point))) "c1"))))

(ert-deftest scooter-kanban-test-quiet-moves ()
  (let ((scooter-kanban-announce-moves nil))
    (scooter-kanban-test-with-board
      (scooter-kanban-test--goto "Fix login")
      (scooter-kanban-move "s3")
      (should-not (seq-find (lambda (c) (string-suffix-p "/messages" (cadr c))) scooter-kanban-test--calls)))))

(ert-deftest scooter-kanban-test-reorder-goes-between-neighbours ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Fix login")
    (scooter-kanban-move-up)
    ;; Above Write docs (1): no refile, no message.
    (should (equal (scooter-kanban-test--writes)
                   '(("PUT" "/conversations/c1/contrib-api/kanban/card" ((column . "s1") (position . 0.5))))))))

(ert-deftest scooter-kanban-test-tied-lane-renumbers ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Fix login")
    (pcase-let* ((`(,board . ,card) (scooter-kanban-at-point))
                 (docs (seq-find (lambda (c) (equal (scooter-kanban-card-id c) "c2")) (plist-get board :cards))))
      ;; Write docs and Root filed both at 0: nothing fits between them.
      (plist-put docs :card nil)
      (setq scooter-kanban-test--calls nil)
      (scooter-kanban-place board card "s1" 1)
      (should (equal (scooter-kanban-test--writes)
                     '(("PUT" "/conversations/c1/contrib-api/kanban/card" ((column . "s1") (position . 1)))
                       ("PUT" "/conversations/c4/contrib-api/kanban/card" ((column . "s1") (position . 2)))))))))

(ert-deftest scooter-kanban-test-tags-split-known-and-new ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Write docs")
    (scooter-kanban-set-tags '("Dotfiles" "emacs" " "))
    (should (equal (scooter-kanban-test--writes)
                   '(("PUT" "/conversations/c2/contrib-api/kanban/card/tags"
                      ((tags . ["t1"]) (board . "b1") (create . ["emacs"]))))))))

(ert-deftest scooter-kanban-test-blockers ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Fix login")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p table &rest _)
                 (should (equal (mapcar #'car table) '("Write docs  [Design]" "Root filed  [Design]" "Shipped  [Done]")))
                 "Shipped  [Done]")))
      (scooter-kanban-add-blocker))
    (should (equal (car (scooter-kanban-test--writes))
                   '("PUT" "/conversations/c1/contrib-api/kanban/card/blockers" ((blocked_by . ["c3" "c5"])))))))

(ert-deftest scooter-kanban-test-new-card-files-then-opens-empty ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "In progress (")
    (cl-letf* ((reply (symbol-function 'scooter-request))
               ((symbol-function 'scooter-request)
                (lambda (s method path &optional body)
                  (if (equal (list method path) '("POST" "/conversations"))
                      (progn (push (list method path body) scooter-kanban-test--calls) '((id . "new1")))
                    (funcall reply s method path body))))
               (opened nil)
               ((symbol-function 'scooter-kanban-show-conversation) (lambda (&rest args) (setq opened args))))
      (scooter-kanban-new)
      (should (equal opened '(home "new1" t "New conversation"))))
    (should (equal (scooter-kanban-test--writes)
                   '(("POST" "/conversations" nil)
                     ("PUT" "/conversations/new1/folder" ((folderId . "s2")))
                     ("PUT" "/conversations/new1/contrib-api/kanban/card" ((column . "s2") (position . 1))))))))

(ert-deftest scooter-kanban-test-add-stage-after-point ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Design (")
    (scooter-kanban-add-stage "Review")
    (should (equal (car (scooter-kanban-test--writes))
                   '("PUT" "/contrib-api/kanban/boards/b1"
                     ((stages . [((id . "s1") (name . "Design")) ((name . "Review"))
                                 ((id . "s2") (name . "In progress")) ((id . "s3") (name . "Done"))])
                      (tags . [((id . "t1") (name . "dotfiles") (color . "blue"))])
                      (skills . [((id . "k1") (name . "Repos") (body . "Use the fork."))])))))))

(ert-deftest scooter-kanban-test-stage-reorder-and-tag-color ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Done (")
    (scooter-kanban-stage-left)
    (should (equal (mapcar (lambda (s) (alist-get 'id s))
                           (alist-get 'stages (nth 2 (car (scooter-kanban-test--writes)))))
                   '("s1" "s3" "s2")))
    (setq scooter-kanban-test--calls nil)
    (scooter-kanban-test--goto "tag    dotfiles")
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "red")))
      (scooter-kanban-color-tag))
    (should (equal (alist-get 'tags (nth 2 (car (scooter-kanban-test--writes))))
                   [((color . "red") (id . "t1") (name . "dotfiles"))]))))

(ert-deftest scooter-kanban-test-conversation-commands-act-on-cards ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Fix login")
    (cl-letf (((symbol-function 'scooter-agent-shell-conversation) #'ignore))
      (should (equal (scooter-conversation-target) '(home "c1"))))
    ;; The list's star redraws this buffer, not a list.
    (scooter-list-star)
    (should (equal (car (scooter-kanban-test--writes))
                   '("PATCH" "/conversations/c1/starred" ((starred . t)))))
    (should (derived-mode-p 'scooter-kanban-mode))
    (should (string-match-p "^Work  5 cards$" (buffer-string)))))

(ert-deftest scooter-kanban-test-goto-card-from-a-shell ()
  (scooter-kanban-test-with-board
    (scooter-kanban (list 'home "c3"))
    (should (equal (scooter-kanban-card-id (cdr (scooter-kanban-at-point))) "c3"))))

(ert-deftest scooter-kanban-test-open-uses-another-window ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "Write docs")
    (let (shown)
      (cl-letf (((symbol-function 'scooter-open)
                 (lambda (server id)
                   (setq shown (list server id agent-shell-display-action))
                   (pop-to-buffer (get-buffer-create "*fake shell*") agent-shell-display-action))))
        (delete-other-windows)
        (scooter-kanban-visit)
        (should (equal (butlast shown) '(home "c2")))
        (should (equal (nth 2 shown) scooter-kanban-display-action))
        (should (equal (buffer-name) "*fake shell*"))
        (should (get-buffer-window "*scooter-kanban*"))
        (kill-buffer "*fake shell*")))))

(ert-deftest scooter-kanban-test-not-enabled ()
  (let ((scooter-servers '((home :url "https://s.example"))))
    (cl-letf (((symbol-function 'scooter-request)
               (lambda (&rest _) (signal 'scooter-http-error '("GET /contrib-api/kanban/boards: 404 {}")))))
      (should (equal (scooter-kanban--fetch 'home) "kanban is not enabled on this server")))))

(ert-deftest scooter-kanban-test-hidden-buffer-refreshes-when-shown ()
  (scooter-kanban-test-with-board
    (setq scooter-kanban--stale t)
    (setq scooter-kanban-test--calls nil)
    (scooter-kanban--shown (selected-window))
    (should scooter-kanban-test--calls)
    (should-not scooter-kanban--stale)))

;;; scooter-kanban-test.el ends here

(defmacro scooter-kanban-test-with-tabs (&rest body)
  "Run BODY on the board with cards opening in tabs, `scooter-open' faked."
  (declare (indent 0))
  `(scooter-kanban-test-with-board
     (require 'agent-shell)
     (let ((scooter-kanban-open-in 'tab)
           (opened nil))
       (cl-letf (((symbol-function 'scooter-open)
                  (lambda (server id)
                    (push (list server id) opened)
                    (let ((buf (get-buffer-create (format "*fake shell %s*" id))))
                      (with-current-buffer buf
                        (setq-local scooter-kanban-test--conversation (list server id)))
                      (display-buffer buf agent-shell-display-action)
                      buf)))
                 ((symbol-function 'scooter-kanban--shell)
                  (lambda (server id) (get-buffer (format "*fake shell %s*" id))))
                 ((symbol-function 'scooter-kanban--shell-buffer-p)
                  (lambda (buf) (string-prefix-p "*fake shell" (buffer-name buf))))
                 ((symbol-function 'agent-shell-subscribe-to) #'ignore))
         (tab-bar-mode 1)
         (switch-to-buffer "*scooter-kanban*")
         (delete-other-windows)
         (unwind-protect (progn ,@body)
           (let ((scooter-kanban--closing t))
             (dolist (b (match-buffers "\\`\\*fake shell")) (kill-buffer b)))
           (while (cdr (funcall tab-bar-tabs-function)) (tab-bar-close-tab))
           (tab-bar-mode -1))))))

(defvar-local scooter-kanban-test--conversation nil)

(defun scooter-kanban-test--tab-names ()
  (mapcar (lambda (tab) (alist-get 'name tab)) (funcall tab-bar-tabs-function)))

(ert-deftest scooter-kanban-test-tabs-open-and-reuse ()
  (scooter-kanban-test-with-tabs
    (scooter-kanban-test--goto "Write docs")
    (scooter-kanban-visit)
    ;; Its own tab, named after the card, showing only the chat.
    (should (equal (buffer-name) "*fake shell c2*"))
    (should (one-window-p t))
    (should (equal (alist-get 'name (tab-bar--current-tab)) "Write docs"))
    (should (= (length (funcall tab-bar-tabs-function)) 2))
    ;; Opening it again from the board switches to that tab, no new one.
    (tab-bar-switch-to-recent-tab)
    (should (eq (window-buffer) (get-buffer "*scooter-kanban*")))
    (scooter-kanban-test--goto "Write docs")
    (scooter-kanban-visit)
    (should (equal (buffer-name) "*fake shell c2*"))
    (should (= (length (funcall tab-bar-tabs-function)) 2))
    (should (equal opened '((home "c2"))))))

(ert-deftest scooter-kanban-test-tabs-display-stays-on-board ()
  (scooter-kanban-test-with-tabs
    (scooter-kanban-test--goto "Fix login")
    (scooter-kanban-display)
    (should (eq (window-buffer) (get-buffer "*scooter-kanban*")))
    (should (member "Fix login" (scooter-kanban-test--tab-names)))))

(ert-deftest scooter-kanban-test-tabs-close-together ()
  (scooter-kanban-test-with-tabs
    ;; Closing the tab kills its shell.
    (scooter-kanban-test--goto "Write docs")
    (scooter-kanban-visit)
    (tab-bar-close-tab)
    (should-not (get-buffer "*fake shell c2*"))
    (should (equal (scooter-kanban-test--tab-names) (list (alist-get 'name (tab-bar--current-tab)))))
    ;; Killing the shell closes its tab, here and in the background.
    (scooter-kanban-test--goto "Fix login")
    (scooter-kanban-visit)
    (kill-buffer "*fake shell c1*")
    (should (= (length (funcall tab-bar-tabs-function)) 1))
    (should (eq (window-buffer) (get-buffer "*scooter-kanban*")))
    (set-buffer (window-buffer))
    (scooter-kanban-test--goto "Write docs")
    (scooter-kanban-display)
    (kill-buffer "*fake shell c2*")
    (should (= (length (funcall tab-bar-tabs-function)) 1))))

(ert-deftest scooter-kanban-test-tabs-board-from-shell ()
  (scooter-kanban-test-with-tabs
    (scooter-kanban-test--goto "Write docs")
    (scooter-kanban-visit)
    ;; From the chat, the board comes back in its tab with point on the card.
    (scooter-kanban (list 'home "c2"))
    (should (eq (window-buffer) (get-buffer "*scooter-kanban*")))
    (should (= (length (funcall tab-bar-tabs-function)) 2))
    (should (equal (scooter-kanban-card-id (cdr (scooter-kanban-at-point))) "c2"))
    ;; Stepping through cards peeks in a window, opening no tabs.
    (scooter-kanban-next-card)
    (should (= (length (funcall tab-bar-tabs-function)) 2))))

(ert-deftest scooter-kanban-test-fold-levels ()
  ;; Board, stage and card are levels 2, 3 and 4: each folds to show the next.
  (scooter-kanban-test-with-board
    (cl-flet ((shown (text) (progn (goto-char (point-min)) (search-forward text)
                                   (not (invisible-p (match-beginning 0))))))
      (magit-section-show-level-2-all)
      (should (shown "Work  5 cards"))
      (should-not (shown "  Design (3)"))
      (magit-section-show-level-3-all)
      (should (shown "  Design (3)"))
      (should-not (shown "Write docs"))
      (magit-section-show-level-4-all)
      (should (shown "Write docs"))
      (scooter-kanban-test--goto "Write docs")
      (should (oref (magit-current-section) hidden)))))

(ert-deftest scooter-kanban-test-folds-hide-rows-and-survive-refresh ()
  ;; Folding hides the rows, not just marks the section; a redraw keeps
  ;; each fold as the user left it; every foldable heading bears its mark.
  (let ((magit-section-visibility-indicators '((magit-fringe-bitmap> . magit-fringe-bitmapv) (?> . ?v))))
    (scooter-kanban-test-with-board
      (cl-flet ((hidden-p (text) (progn (goto-char (point-min)) (search-forward text)
                                        (invisible-p (match-beginning 0))))
                (marked-p (text) (progn (scooter-kanban-test--goto text)
                                        (seq-some (lambda (o) (overlay-get o 'magit-vis-indicator))
                                                  (overlays-in (point) (line-end-position))))))
        ;; Hooks start folded, and folded means out of sight.
        (should (hidden-p "finish-or-ask"))
        (should (marked-p "Hooks  2"))
        (should (marked-p "Design (3)"))
        (should (marked-p "Write docs"))
        (should-not (marked-p "E edits"))
        (scooter-kanban-test--goto "Design (3)")
        (magit-section-toggle (magit-current-section))
        (should (hidden-p "Write docs"))
        (scooter-kanban-refresh)
        (should (hidden-p "Write docs"))
        (should (marked-p "Design (3)"))
        (scooter-kanban-test--goto "Design (3)")
        (magit-section-toggle (magit-current-section))
        (should-not (hidden-p "Write docs"))
        (scooter-kanban-refresh)
        (should-not (hidden-p "Write docs"))))))

;;;; Personal skills and hooks

(defun scooter-kanban-test--edit-buffer (name)
  "The edit buffer NAME, which the last command popped up."
  (or (get-buffer name) (ert-fail (format "no buffer %s" name))))

(ert-deftest scooter-kanban-test-renders-user-skills-and-hooks ()
  (scooter-kanban-test-with-board
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      ;; After the boards: each server's own skills, then its hooks.
      (should (string-match-p "E edits stages, tags and skills\n\nSkills  2\n  ● acme-conventions\n    Use in acme/app.\n" text))
      (should (string-match-p "  ○ github-workflow  disabled  replaces a built-in\n    Old notes.\n" text))
      (should (string-match-p "Hooks  2\n  ● finish-or-ask  Stop  required\n    Back to work instead of ending.\n  ● lint  PostToolUse\n" text)))
    ;; Hooks start folded; skills don't.
    (scooter-kanban-test--goto "Hooks  2")
    (should (oref (magit-current-section) hidden))
    (scooter-kanban-test--goto "Skills  2")
    (should-not (oref (magit-current-section) hidden))))

(ert-deftest scooter-kanban-test-user-sections-follow-the-server ()
  (let ((replies (symbol-function 'scooter-kanban-test--reply)))
    (cl-letf (((symbol-function 'scooter-kanban-test--reply)
               (lambda (method path body)
                 (pcase path
                   ("/user-skills" (signal 'scooter-http-error '("GET /user-skills: 501 {}")))
                   ("/user-hooks" (signal 'scooter-http-error '("GET /user-hooks: 404 {}")))
                   (_ (funcall replies method path body))))))
      (scooter-kanban-test-with-board
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "Skills\n  Personal skills: not configured on this server\n" text))
          (should-not (string-match-p "Hooks" text)))))))

(ert-deftest scooter-kanban-test-new-user-skill-is-a-file ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "home  https")
    (scooter-kanban-new-user-skill (scooter-kanban-server-at-point))
    (with-current-buffer (scooter-kanban-test--edit-buffer "*scooter-skill: new@home*")
      (should (equal (buffer-string) "---\nname: \ndescription: \nenabled: true\n---\n\n"))
      (erase-buffer)
      (insert "---\nname: acme-release\ndescription: Shipping acme/app.\nenabled: true\n---\n\n# Release\n\nTag it.\n")
      (scooter-kanban-text-save))
    (should-not (get-buffer "*scooter-skill: new@home*"))
    (should (equal (scooter-kanban-test--writes)
                   '(("POST" "/user-skills"
                      ((name . "acme-release") (description . "Shipping acme/app.")
                       (body . "# Release\n\nTag it.") (enabled . t))))))))

(ert-deftest scooter-kanban-test-new-user-skill-needs-its-front-matter ()
  (scooter-kanban-test-with-board
    (scooter-kanban-new-user-skill 'home)
    (with-current-buffer (scooter-kanban-test--edit-buffer "*scooter-skill: new@home*")
      (unwind-protect
          (progn
            (erase-buffer)
            (insert "---\nname: \ndescription: Shipping.\n---\nBody")
            (should-error (scooter-kanban-text-save) :type 'user-error)
            (erase-buffer)
            (insert "Just a body")
            (should-error (scooter-kanban-text-save) :type 'user-error)
            ;; The buffer stays, with its text, for another try.
            (should (equal (buffer-string) "Just a body")))
        (kill-buffer)))
    (should-not (scooter-kanban-test--writes))))

(ert-deftest scooter-kanban-test-edit-user-skill-at-point ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "github-workflow")
    (scooter-kanban-edit-at-point)
    (with-current-buffer (scooter-kanban-test--edit-buffer "*scooter-skill: github-workflow@home*")
      (should (equal (buffer-string)
                     "---\nname: github-workflow\ndescription: Old notes.\nenabled: false\n---\n\nPush to main."))
      (goto-char (point-max))
      (insert " Never.")
      (scooter-kanban-text-save))
    (should (equal (scooter-kanban-test--writes)
                   '(("PATCH" "/user-skills/u2"
                      ((name . "github-workflow") (description . "Old notes.")
                       (body . "Push to main. Never.") (enabled . :json-false))))))))

(ert-deftest scooter-kanban-test-toggle-user-skill-and-hook ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "github-workflow")
    (scooter-kanban-toggle)
    (should (equal (scooter-kanban-test--writes)
                   '(("PATCH" "/user-skills/u2" ((enabled . t))))))
    (setq scooter-kanban-test--calls nil)
    (scooter-kanban-test--goto "lint  PostToolUse")
    (scooter-kanban-visit)
    (should (equal (scooter-kanban-test--writes)
                   '(("PATCH" "/user-hooks/lint" ((enabled . :json-false))))))
    (setq scooter-kanban-test--calls nil)
    (scooter-kanban-test--goto "finish-or-ask")
    (should-error (scooter-kanban-toggle) :type 'user-error)
    (should-not (scooter-kanban-test--writes))
    (scooter-kanban-test--goto "Write docs")
    (should-error (scooter-kanban-toggle) :type 'user-error)))

(ert-deftest scooter-kanban-test-delete-user-skill ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "acme-conventions")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (scooter-kanban-delete-user-skill))
    (should (equal (scooter-kanban-test--writes) '(("DELETE" "/user-skills/u1" nil))))))

(ert-deftest scooter-kanban-test-board-skill-edits-as-a-file ()
  (scooter-kanban-test-with-board
    (scooter-kanban-test--goto "skill  Repos")
    (scooter-kanban-edit-at-point)
    (with-current-buffer (scooter-kanban-test--edit-buffer "*scooter-skill: Repos (Work)*")
      (should (equal (buffer-string) "---\nname: Repos\n---\n\nUse the fork."))
      (erase-buffer)
      (insert "---\nname: Repositories\n---\nUse the fork, always.")
      (scooter-kanban-text-save))
    (should (equal (alist-get 'skills (nth 2 (car (scooter-kanban-test--writes))))
                   [((body . "Use the fork, always.") (name . "Repositories") (id . "k1"))]))
    (setq scooter-kanban-test--calls nil)
    (scooter-kanban-test--goto "Work  5 cards")
    (scooter-kanban-add-skill)
    (with-current-buffer (scooter-kanban-test--edit-buffer "*scooter-skill: new (Work)*")
      (erase-buffer)
      (insert "---\nname: Shipping\n---\nOpen a PR.")
      (scooter-kanban-text-save))
    (should (equal (alist-get 'skills (nth 2 (car (scooter-kanban-test--writes))))
                   [((id . "k1") (name . "Repos") (body . "Use the fork.")) ((name . "Shipping") (body . "Open a PR."))]))))

(ert-deftest scooter-kanban-test-front-matter-round-trips ()
  (should (equal (scooter-kanban-edit-parse "---\nname: a-b\ndescription: When x: y.\nenabled: false\n---\n\n# A\n\nBody.\n")
                 '((("name" . "a-b") ("description" . "When x: y.") ("enabled" . "false")) . "# A\n\nBody.")))
  (should (equal (scooter-kanban-edit-parse "---\n---\nBody") '(nil . "Body")))
  (should (equal (scooter-kanban-edit-parse "No front matter\n") '(nil . "No front matter")))
  (should (equal (scooter-kanban-edit-parse "---\nname: only\n---") '((("name" . "only")) . "")))
  (should (equal (scooter-kanban-edit-front-matter '(("name" . "x") ("enabled" . "true")))
                 "---\nname: x\nenabled: true\n---\n\n"))
  (should (scooter-kanban-edit-true-p nil))
  (should (scooter-kanban-edit-true-p "True"))

  (should-not (scooter-kanban-edit-true-p " false ")))

;;;; Opening while not signed in

(ert-deftest scooter-kanban-test-cancelled-first-open-starts-over ()
  ;; C-g during the first fetch (a sign-in wait): no empty board is left
  ;; behind, and the next open fetches again.
  (let ((scooter-servers '((home :url "https://s.example")))
        (calls 0))
    (cl-letf (((symbol-function 'scooter-request)
               (lambda (_server method path &optional body)
                 (cl-incf calls)
                 (if (= calls 1) (signal 'quit nil) (scooter-kanban-test--reply method path body))))
              ((symbol-function 'scooter-kanban--start-streams) #'ignore))
      (when (get-buffer "*scooter-kanban*") (kill-buffer "*scooter-kanban*"))
      (save-window-excursion
        ;; `should-error' handles `error' only; `quit' isn't one.
        (should (eq (condition-case nil (progn (scooter-kanban) 'returned) (quit 'quit)) 'quit))
        (should-not (get-buffer "*scooter-kanban*"))
        (scooter-kanban)
        (unwind-protect
            (with-current-buffer "*scooter-kanban*"
              (should (> calls 1))
              (should (save-excursion (goto-char (point-min)) (search-forward "Fix login" nil t))))
          (kill-buffer "*scooter-kanban*"))))))

(ert-deftest scooter-kanban-test-failed-open-retries-on-the-next-open ()
  ;; A sign-in that timed out shows its error; the next open fetches again.
  (let ((scooter-servers '((home :url "https://s.example")))
        (fail t))
    (cl-letf (((symbol-function 'scooter-request)
               (lambda (_server method path &optional body)
                 (if fail
                     (user-error "Scooter[home]: sign-in not finished after 300 seconds")
                   (scooter-kanban-test--reply method path body))))
              ((symbol-function 'scooter-kanban--start-streams) #'ignore))
      (when (get-buffer "*scooter-kanban*") (kill-buffer "*scooter-kanban*"))
      (save-window-excursion
        (scooter-kanban)
        (unwind-protect
            (progn
              (with-current-buffer "*scooter-kanban*"
                (should scooter-kanban--failed)
                (should (save-excursion (goto-char (point-min)) (search-forward "not finished" nil t))))
              (setq fail nil)
              (scooter-kanban)
              (with-current-buffer "*scooter-kanban*"
                (should-not scooter-kanban--failed)
                (should (save-excursion (goto-char (point-min)) (search-forward "Fix login" nil t)))))
          (kill-buffer "*scooter-kanban*"))))))

(ert-deftest scooter-kanban-test-streams-and-timers-never-sign-in ()
  ;; Change streams and the refreshes they trigger run with auth quiet:
  ;; not being signed in is no reason to open the sign-in page.
  (scooter-kanban-test-with-board
    (let (quiet)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'scooter-http-stream)
                       (lambda (&rest _) (push scooter-auth-quiet quiet) (error "down"))))
              (scooter-kanban--follow (current-buffer) 'home)
              (should (equal quiet '(t)))
              ;; A stream that couldn't start is tried again later.
              (should (seq-some (lambda (timer) (eq (timer--function timer) #'scooter-kanban--follow))
                                timer-list)))
            (setq quiet nil)
            (cl-letf (((symbol-function 'scooter-kanban-refresh)
                       (lambda () (push scooter-auth-quiet quiet))))
              (setq scooter-kanban--stale t)
              (scooter-kanban--shown (selected-window))
              (should (equal quiet '(t)))))
        (cancel-function-timers #'scooter-kanban--follow)))))

;;; scooter-kanban-test.el ends here
