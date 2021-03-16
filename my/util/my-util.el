;;; my-util.el --- Utility functions - my moi, for moi -*- lexical-binding: t; -*-

(defun my-util-ensure-dir (&rest dirs)
  "Join args to single path and create directory if it does not exist."
  (let ((path (apply 'f-join dirs)))
    (f-mkdir path)
    path))

(defmacro my-util-with-eval-after-frame (&rest body)
  `(if
       (daemonp)
       (add-hook
        'after-make-frame-functions
        (function
         (lambda (frame)
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
    (call-interactively
     'sort-lines)))

(defun my-util-base64-encode-region-no-break ()
  (interactive)
  (base64-encode-region
   (mark)
   (point)
   t))

(defun my-util-remove-newlines-in-region ()
  (interactive)
  (save-restriction
    (narrow-to-region
     (point)
     (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "" nil t))))

(defun my-util-indent-rigidly (start end arg &optional interactive)
  "Move pointer to start ofIndent rigidily do not skip first line when indenting"
  (interactive "r\nP\np")
  (save-excursion
    (when (use-region-p)
      (goto-char start))
    (beginning-of-line)
    (set-mark (point))
    (call-interactively
     'indent-rigidly)))


;; https://stackoverflow.com/questions/46017956/emacs-how-to-change-kill-to-delete
(defmacro my-util--delete-instead-of-kill (&rest body)
  "Replaces `kill-region' with `delete-region' in BODY."
  `(cl-letf
       (((symbol-function 'kill-region)
         (lambda (beg end &optional yank-handler)
           (delete-region beg end))))
     ,@body))

;; Otherwise backward-kill-sexp used by selectrum bloats the kill ring
(defun my-util-backward-delete-sexp (arg)
  "Like `kill-word', but does not save to the `kill-ring'."
  (interactive "*p")
  (my-util--delete-instead-of-kill
   (backward-kill-sexp arg)))


(defun my-util-discard-buffer-action (buf)
  (with-current-buffer buf
    (if (file-exists-p buffer-file-name)
        (revert-buffer :ignore-auto :noconfirm)
      (set-buffer-modified-p nil)
      (kill-buffer buf))))

(provide 'my-util)
;;; my-util.el ends here
