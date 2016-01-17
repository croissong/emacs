(require 'org)
(org-babel-tangle-file (concat user-emacs-directory "init.org"))
(byte-compile-file (concat user-emacs-directory "init_temp.el"))
(rename-file (concat user-emacs-directory "init_temp.elc")
	     (concat user-emacs-directory "init.elc")
	     t)
(delete-file (concat user-emacs-directory "init_temp.el"))
(load-file (concat user-emacs-directory "init.elc"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#efece3"))))
 '(org-block-begin-line ((t (:foreground "#446a5d"))))
 '(org-block-end-line ((t (:foreground "#446a5d"))))
 '(org-level-2 ((t (:foreground "cadet blue"))))
 '(org-level-3 ((t (:foreground "#b75761"))))
 '(org-level-4 ((t (:foreground "darkorange"))))
 '(org-link ((t (:foreground "bisque4"))))
 '(org-property-value ((t (:foreground "purple"))) t)
 '(org-special-keyword ((t (:foreground "#990099")))))
