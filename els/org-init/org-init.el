;;; org-init.el --- Write init in emacs org-mode  -*- lexical-binding: t; -*-
;;; Commentary: 
;;; Code:
(require 'org)
(require 'bytecomp)
(require 'byte-compile)

(defvar org-init--init-org (expand-file-name "init.org" user-emacs-directory))
(defvar org-init--init-el (expand-file-name "init.el" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun org-tangle-staged (in-file out-file)
  (let* ((name (concat (file-name-sans-extension in-file) "_staged"))
         (ext (file-name-extension in-file))
         (staged-file (make-temp-file name nil (concat "." ext))))
    (with-temp-file staged-file
      (call-process-shell-command (concat "git show :" in-file) nil t))
    (org-babel-tangle-file staged-file out-file)
    )
  )

(defun org-tangle-all (in-file out-file)
  (org-babel-tangle-file in-file out-file))

(defun org-init-compile ()
  (interactive)
  (org-tangle-all "init.org" org-init--init-el)
  )

(defun org-init-git ()
  (interactive)
  (magit-status-internal user-emacs-directory))

(defun org-init-open () 
  (interactive)
  (find-file org-init--init-org))

(provide 'org-init)

;;; org-init.el ends here
