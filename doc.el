;; update readme with contents of help command
(require 'dash)
(require 's)

(defun sig--text-split (cmd)
  (let* ((raw (split-string (shell-command-to-string (concat "sig help " cmd)) "\n"))
         (lines (-map (lambda (line)
                        (if (string-prefix-p "--" line)
                            (let ((cols (s-split-up-to "   +" line 2)))
                              (concat "- =" (car cols) "= " (cadr cols)))
                          line))
                      raw)))
    lines))

(defun sig--insert-help ()
  (let ((cmds (sort (cdr (split-string (shell-command-to-string "sig version|tail -n +2")))
                    'string-lessp)))
    (-each cmds
      (lambda (c)
        (insert "** " c "\n\n")
        (insert (s-join "\n" (sig-text-split c)))
        (insert "\n")))))

(defun sig--remove-help ()
  (search-forward-regexp "^# -- generated doc")
  (forward-line 1)
  (let ((start (point)))
    (search-forward-regexp "^\\* .+")
    (forward-line -1)
    (delete-region start (point))))

(defun sig-update-readme ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (and (eq major-mode 'org-mode)
                 (looking-at "^#\\+TITLE: simple image gallery$"))
      (user-error "Not the correct file."))
    (sig-remove-help)
    (sig-insert-help)))
