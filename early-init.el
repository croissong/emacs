;;; early-init.el -*- lexical-binding: t; -*-

;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

(setq gc-cons-threshold most-positive-fixnum
      native-comp-deferred-compilation nil
      package-enable-at-startup nil
      load-prefer-newer noninteractive
      default-input-method nil)

(set-language-environment "UTF-8")
