;;; org-init.el --- Write init in emacs org-mode  -*- lexical-binding: t; -*-
;;; Commentary: 
;;; Code:
(require 'org)

(defvar org-init--file (expand-file-name "init.org" user-emacs-directory))
(defvar org-init--el-file (expand-file-name "org-init.el" user-emacs-directory))
(defvar org-init--elc-file (expand-file-name "init.elc" user-emacs-directory))

(defsubst org-init--make-keymap ()
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "C-t C-c") 'org-init-compile)
    keymap))

(define-minor-mode org-init--mode
  "for save and tangle "
  nil " stc" (org-init--make-keymap))

(defun org-init--tangle ()
  (org-babel-tangle-file org-init--file org-init--el-file))

(defun org-init-recompile ()
  (interactive)
  (if (org-init--need-compile?)
      (progn
        (org-init--tangle)
        (message "compiling file...")
	(byte-compile-file org-init--el-file)
	(rename-file (concat org-init--el-file "c") org-init--elc-file t)
        (message "compiled, will be loaded next time"))
    (message "no need to recompile")
    nil))

(defun org-init--need-compile? ()
  (or (not (file-exists-p org-init--elc-file))
      (file-newer-than-file-p org-init--file org-init--elc-file)))

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
