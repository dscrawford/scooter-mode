;;; check-commands.el --- Fail on keys and menus bound to undefined commands  -*- lexical-binding: t; -*-

;; Usage: emacs -Q --batch -L lisp -l test/check-commands.el FILE.el
;; Installed as the README shows, from `load-path' with no generated autoloads,
;; a command bound in FILE's keymaps or transient menus has to be defined or
;; `autoload'ed once FILE is loaded, or its key fails.

(let* ((file (car command-line-args-left))
       (cmds nil)
       (bad nil))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    ;; Keys, "k" #'scooter-… or (kbd "k") #'scooter-…, and transient
    ;; suffixes, ("k" "Label" scooter-…).
    (while (re-search-forward (concat "\"[^\"\n]*\")? +#'\\(scooter-[[:alnum:]-]+\\)"
                                      "\\|^ *(\"[^\"]+\" \"[^\"]*\" \\(scooter-[[:alnum:]-]+\\))")
                              nil t)
      (push (intern (or (match-string 1) (match-string 2))) cmds)))
  (load (expand-file-name file) nil t)
  (dolist (cmd (delete-dups cmds))
    (unless (fboundp cmd) (push (symbol-name cmd) bad)))
  (when bad
    (message "%s binds commands nothing defines or autoloads: %s"
             (file-name-nondirectory file) (string-join (nreverse bad) ", "))
    (kill-emacs 1)))
