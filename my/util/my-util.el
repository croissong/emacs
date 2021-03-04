;;; my-util.el --- Utility functions  -*- lexical-binding: t; -*-

(defun my-util-ensure-emacs-subdir (path)
  (let ((expanded-path (expand-file-name path user-emacs-directory )))
    (unless (file-directory-p expanded-path)
      (mkdir expanded-path t))
    expanded-path))

(defmacro my-util-with-eval-after-frame (&rest body)
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (function (lambda (frame)
                   (select-frame frame)
                   ,@body)))
     ,@body))

(defun my-util-backward-delete-whitespace-or-word ()
  (interactive)
  (if (looking-back "\\(\t\\|  \\)")
      (delete-horizontal-space)
    (backward-kill-word 1)))

(defun my-util-capitalize-previous-word()
  (interactive)
  (capitalize-word -1))

(defun my-util-sort-lines-case-insensitive ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun my-util-base64-encode-region-no-break ()
  (interactive)
  (base64-encode-region (mark) (point) t))

(defun my-util-remove-newlines-in-region ()
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

(defun my-util-indent-rigidly(start end arg &optional interactive)
  "Move pointer to start ofIndent rigidily do not skip first line when indenting"
  (interactive "r\nP\np")
  (save-excursion
    (when (use-region-p)
      (goto-char start))
    (beginning-of-line)
    (set-mark (point))
    (call-interactively 'indent-rigidly)))

(provide 'my-util)
;;; my-util.el ends here
