;;; org-init.el --- Write init in emacs org-mode  -*- lexical-binding: t; -*-
;;; Commentary: 
;;; Code:
(require 'org)
(require 'bytecomp)
(require 'byte-compile)

(defvar org-init--init-org (expand-file-name "init.org" user-emacs-directory))
(defvar org-init--org-init-el (expand-file-name "org-init.el" user-emacs-directory))
(defvar org-init--init-el (expand-file-name "init.el" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defsubst org-init--make-keymap ()
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "C-t C-c") 'org-init-compile)
    keymap))

(define-minor-mode org-init--mode
  "for save and tangle "
  nil " stc" (org-init--make-keymap))

(defun org-init--tangle ()
  (org-babel-tangle-file org-init--init-org org-init--org-init-el))

(defun org-init-recompile ()
  (interactive) 
  (when (org-init--need-compile? org-init--init-el
				 (concat org-init--init-el "c"))
    (byte-compile-file org-init--init-el))
  (when (org-init--need-compile? org-init--init-org
				 (concat org-init--org-init-el "c"))
    (org-init--tangle)
    (when (byte-compile-file org-init--org-init-el)
      (delete-file org-init--org-init-el)
      t)))

(defun org-init-load ()
  (load-file (concat org-init--org-init-el "c")))

(defun org-init--need-compile? (file dest)
  (or (not (file-exists-p dest))
      (file-newer-than-file-p file
                              dest)))

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
