;;; early-init.el -*- lexical-binding: t; -*-

;; bootstrap
(unless (file-exists-p (expand-file-name "init.el" user-emacs-directory))
  (use-package org)
  (org-babel-tangle-file (expand-file-name "init.org" user-emacs-directory)))


;; mostly based on:
;; https://github.com/doomemacs/doomemacs/blob/master/early-init.el
;; => calls https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-start.el
;; also for reference wrt package settings:
;; https://github.com/progfolio/elpaca/blob/master/doc/early-init.el

(set-language-environment "UTF-8")

(setq gc-cons-threshold most-positive-fixnum
      package-enable-at-startup nil
      load-prefer-newer t
      default-input-method nil
      selection-coding-system 'utf-8)

;;; early-init.el ends here
