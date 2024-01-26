;;; early-init.el -*- lexical-binding: t; -*-

;; bootstrap
(unless (file-exists-p (expand-file-name "init.el" user-emacs-directory))
  (use-package org)
  (org-babel-tangle-file (expand-file-name "init.org" user-emacs-directory)))


;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
(setq gc-cons-threshold most-positive-fixnum
      package-enable-at-startup nil
      load-prefer-newer noninteractive
      default-input-method nil)

(set-language-environment "UTF-8")
