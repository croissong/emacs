;;; org-init.el --- Write init in emacs org-mode  -*- lexical-binding: t; -*-
;;; Commentary: 
;;; Code:
(require 'org)
(require 'bytecomp)
(require 'byte-compile)

(defvar org-init--init-org (expand-file-name "init.org" user-emacs-directory))
(defvar org-init--init-el (expand-file-name "init.el" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun org-init-compile ()
  (interactive)
  (org-babel-tangle-file org-init--init-org org-init--init-el)
  (byte-compile-file org-init--init-el)
  )

(defun org-init-git ()
  (interactive)
  (magit-status-internal user-emacs-directory))

(defun org-init-open () 
  (interactive)
  (with-current-buffer (find-file org-init--init-org)
    (org-init--mode 1)
    (diminish 'org-init--mode)))

(provide 'org-init)

;;; org-init.el ends here
