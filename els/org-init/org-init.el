;;; org-init.el --- Write journal in emacs org-mode  -*- lexical-binding: t; -*-

(require 'org)
(defvar org-init--file (concat user-emacs-directory "init.org"))
(defvar org-init--temp-file (concat user-emacs-directory "init_temp.el"))

(defsubst org-init--make-keymap ()
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "C-t C-c") 'org-init-compile)
    keymap))

(define-minor-mode org-init--mode
  "for save and tangle "
  nil " stc" (org-init--make-keymap))


(defun org-init-compile ()
  (interactive)
  (org-babel-tangle-file org-init--file org-init--temp-file)
  (byte-compile-file org-init--temp-file t)
  (rename-file (concat user-emacs-directory "init_temp.elc")
	       (concat user-emacs-directory "init.elc")
	       t)
  (delete-file org-init--temp-file))

(defun org-init-git ()
  (interactive)
  (magit-status-internal user-emacs-directory))

(defun org-init-open () 
  (interactive)
  (with-current-buffer (find-file org-init--file)
    (org-init--mode 1)
    (diminish 'org-init--mode)))
(provide 'org-init)

;;; org-init.el ends here
