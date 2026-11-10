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
      (should (string-match-p "description +Make the board work.\n +In Emacs." text))
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

(ert-deftest scooter-kanban-test-new-card-files-then-sends ()
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
      (scooter-kanban-new "Do the thing")
      (should (equal opened '(home "new1" t))))
    (should (equal (scooter-kanban-test--writes)
                   '(("POST" "/conversations" nil)
                     ("PUT" "/conversations/new1/folder" ((folderId . "s2")))
                     ("PUT" "/conversations/new1/contrib-api/kanban/card" ((column . "s2") (position . 1)))
                     ("POST" "/conversations/new1/messages" ((text . "Do the thing"))))))))

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
