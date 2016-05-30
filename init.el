;; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/els/org-init/")
(require 'org-init)
;(org-init-compile)
(org-init-load t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(package-selected-packages
   (quote
    (which-key xtest window-purpose web-mode undo-tree ssh-agency sql-indent spaceline soft-stone-theme smex smartparens scss-mode request-deferred req-package projectile org-plus-contrib neotree navi-mode nameless multiple-cursors markdown-mode magit js2-mode htmlize google-translate flycheck flx-ido floobits expand-region evil-nerd-commenter esqlite elpy ein drag-stuff dired+ company-web coffee-mode centered-window-mode buffer-move bitly auto-package-update auctex alchemist aggressive-indent adaptive-wrap))))
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
