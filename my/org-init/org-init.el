;;; org-init.el --- Write init in emacs org-mode  -*- lexical-binding: t; -*-
;;; Commentary: 
;;; Code:
(require 'org)

(defcustom org-init-init-org (expand-file-name "init.org" user-emacs-directory) "Init file")
(defvar org-init--init-el (expand-file-name "init.el" user-emacs-directory))

;;;###autoload
(define-minor-mode org-init-mode
  "Compile before save"
  :global nil
  (if org-init-mode
      (add-hook 'after-save-hook 'org-init--compile nil 'local)
    (remove-hook 'after-save-hook 'org-init--compile 'local)))

(defun org-init--compile ()
  (org-babel-tangle-file org-init-init-org org-init--init-el))

;;;###autoload
(defun org-init-open () 
  (interactive)
  (find-file org-init-init-org))

(provide 'org-init)

;;; org-init.el ends here
