;;; org-init.el --- Write journal in emacs org-mode  -*- lexical-binding: t; -*-

(defvar org-init--file (concat user-emacs-directory "init.org"))

(defsubst org-init--make-keymap ()
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "C-t C-c") 'org-init-tangle-compile)
    keymap))

(define-minor-mode org-init--mode
  "for save and tangle "
  nil " stc" (org-init--make-keymap))


(defun org-init-tangle-compile ()
  (interactive)
  (save-buffer)
  (org-babel-tangle)
  (byte-compile-file (concat user-emacs-directory "init_temp.el"))
  (rename-file (concat user-emacs-directory "init_temp.elc")
	       (concat user-emacs-directory "init.elc")
	       t)
  (delete-file (concat user-emacs-directory "init_temp.el")))


(defun org-init-open () 
  (interactive)
  (with-current-buffer (find-file org-init--file)
    (org-init--mode 1)
    (diminish 'org-init--mode)))
(provide 'org-init)

;;; org-init.el ends here
