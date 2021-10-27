;;; my.el --- Utility functions - my moi, for moi -*- lexical-binding: t; -*-

(defun my-ensure-dir (&rest dirs)
  "Join args to single path and create directory if it does not exist."
  (let ((path (apply 'f-join dirs)))
    (f-mkdir path)
    path))

(defmacro my-with-eval-after-frame (&rest body)
  `(if
       (daemonp)
       (add-hook
        'after-make-frame-functions
        (function
         (lambda (frame)
           (select-frame frame)
           ,@body)))
     ,@body))

(defun my-backward-delete-whitespace-or-word ()
  (interactive)
  (if (looking-back "\\(\t\\|  \\)")
      (delete-horizontal-space)
    (backward-kill-word 1)))

(defun my-capitalize-previous-word()
  (interactive)
  (capitalize-word -1))

(defun my-sort-lines-case-insensitive ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun my-base64-encode-region-no-break ()
  (interactive)
  (base64-encode-region
   (mark)
   (point)
   t))

(defun my-remove-newlines-in-region ()
  (interactive)
  (save-restriction
    (narrow-to-region
     (point)
     (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "" nil t))))

(defun my-indent-rigidly (start end arg &optional interactive)
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
(defmacro my--delete-instead-of-kill (&rest body)
  "Replaces `kill-region' with `delete-region' in BODY."
  `(cl-letf
       (((symbol-function 'kill-region)
         (lambda (beg end &optional yank-handler)
           (delete-region beg end))))
     ,@body))

;; Otherwise backward-kill-sexp used by selectrum bloats the kill ring
(defun my-backward-delete-sexp (arg)
  "Like `kill-word', but does not save to the `kill-ring'."
  (interactive "*p")
  (my--delete-instead-of-kill
   (backward-kill-sexp arg)))


(defun my-discard-buffer-action (buf)
  (with-current-buffer buf
    (if (file-exists-p buffer-file-name)
        (revert-buffer :ignore-auto :noconfirm)
      (set-buffer-modified-p nil)
      (kill-buffer buf))))

(define-minor-mode babel-tangle-mode
  "Tangle after save"
  :global nil
  :lighter " tangle"
  (if babel-tangle-mode
      (add-hook 'after-save-hook 'org-babel-tangle nil 'local)
    (remove-hook 'after-save-hook 'org-babel-tangle 'local)))


;; How to make org-mode org-insert-link (C-c C-l) automatically fill in the description from a webpage:
(defun my-url-get-title (url &optional descr)
  "Takes a URL and returns the value of the <title> HTML tag,
   Thanks to https://frozenlock.org/tag/url-retrieve/ for documenting url-retrieve"
  (let ((buffer (url-retrieve-synchronously url))
        (title nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (search-forward-regexp "<title>\\([^<]+?\\)</title>")
      (setq title (match-string 1 ) )
      (kill-buffer (current-buffer)))
    title))


(defun my-open-in-buffer (buffer txt)
  "create a new buffer with name <buffer>, insert <txt>"
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name buffer)))
  (insert txt))

;; Usage:
;; (advice-add 'clean-buffer-list :around #'make-silent)
(defun make-silent (func &rest args)
  (cl-letf
      (((symbol-function 'message)(lambda (&rest args) nil)))
    (apply func args)))

(defun my-disable-newlines-eof()
  (interactive)
  (setq-local require-final-newline nil))


(provide 'my)
;;; my.el ends here
