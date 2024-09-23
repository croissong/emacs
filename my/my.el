;;; my.el --- Utility functions - by moi, for moi -*- lexical-binding: t; -*-

(require 'request)

(require 'my-snippets)
(require 'my-menus)
(require 'my-tabs)

(defun my-ensure-dir (&rest dirs)
  "Join args to single path and create directory if it does not exist."
  (let ((path (apply 'f-join dirs)))
    (f-mkdir-full-path path)
    path))

(defmacro my-with-eval-after-frame (&rest body)
  `(if (daemonp)
       (add-hook
        'after-make-frame-functions
        (function
         (lambda (frame)
           (select-frame frame)
           ,@body)))
     ,@body))

(defun my-capitalize-previous-word ()
  (interactive)
  (capitalize-word -1))

(defun my-sort-lines-case-insensitive ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun my-base64-encode-region-no-break ()
  (interactive)
  (base64-encode-region (mark) (point) t))

(defun my-remove-newlines-in-region ()
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "" nil t))))

(defun my-indent-rigidly (start _end _arg &optional _interactive)
  "Move pointer to start ofIndent rigidily do not skip first line when indenting"
  (interactive "r\nP\np")
  (save-excursion
    (when (use-region-p)
      (goto-char start))
    (beginning-of-line)
    (set-mark (point))
    (call-interactively 'indent-rigidly)))


(defun my-discard-buffer-action (buf)
  (with-current-buffer buf
    (when (file-exists-p buffer-file-name)
      (revert-buffer :ignore-auto :noconfirm)
      (set-buffer-modified-p nil)
      t)))

(define-minor-mode babel-tangle-mode
  "Tangle after save"
  :global nil
  :lighter
  " tangle"
  (if babel-tangle-mode
      (add-hook 'after-save-hook 'org-babel-tangle nil 'local)
    (remove-hook 'after-save-hook 'org-babel-tangle 'local)))

;; How to make org-mode org-insert-link (C-c C-l) automatically fill in the description from a webpage:
(defun my-url-get-title (url &optional _descr)
  "Takes a URL and returns the value of the <title> HTML tag"
  (let (result)
    (request
     url
     :parser (lambda () (libxml-parse-html-region (point) (point-max)))
     :sync t
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let ((title-elem (dom-by-tag data 'title)))
          (setq result (dom-text title-elem))))))
    result))


(defun my-open-in-buffer (buffer txt)
  "create a new buffer with name <buffer>, insert <txt>"
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name buffer)))
  (insert txt))

;; Usage:
;; (advice-add 'clean-buffer-list :around 'suppress-message-advice-around)
;; undo:
;; (advice-remove 'clean-buffer-list 'suppress-message-advice-around)
(defun suppress-message-advice-around (fun &rest args)
  (let (message-log-max)
    (with-temp-message (or (current-message) "")
      (apply fun args))))

(defun my-disable-newlines-eof ()
  (interactive)
  (setq-local require-final-newline nil))

(defun my-tmp-from-clipboard ()
  (interactive)
  (let* ((ext (read-string "" ".json"))
         (clipboard (car kill-ring))
         (tmpfile (make-temp-file "scratch" nil ext clipboard)))
    (message clipboard)
    (find-file tmpfile)))


(provide 'my)
;;; my.el ends here
