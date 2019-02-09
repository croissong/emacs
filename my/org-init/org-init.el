;;; my/org-init.el --- Write init in emacs org-mode  -*- lexical-binding: t; -*-
;;; Commentary: 
;;; Code:
(require 'org)

(defcustom my/org-init-org-file (expand-file-name "init.org" user-emacs-directory) "Init file")
(defvar my/org-init--el-file (expand-file-name "init.el" user-emacs-directory))

;;;###autoload
(define-minor-mode my/org-init-mode
  "Compile before save"
  :global nil
  :lighter "init"
  (if my/org-init-mode
      (add-hook 'after-save-hook 'my/org-init--compile nil 'local)
    (remove-hook 'after-save-hook 'my/org-init--compile 'local)))

(defun my/org-init--compile ()
  (org-babel-tangle-file my/org-init-org-file my/org-init--el-file))

;;;###autoload
(defun my/org-init-open () 
  (interactive)
  (find-file my/org-init-org-file))

(provide 'my/org-init)

;;; my/org-init.el ends here
