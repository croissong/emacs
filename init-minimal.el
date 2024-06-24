;; -*- lexical-binding: t -*-
;; emacs -q --load ~/.config/emacs/init-minimal.el



(setq initial-scratch-message nil
      inhibit-startup-screen t)


(use-package gif-screencast
  :load-path "elpaca/repos/emacs-gif-screencast/"
  :commands gif-screencast-start-or-stop
  :custom
  (gif-screencast-program "grim")
  (gif-screencast-args ()))

(global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "M-d") 'switch-to-buffer)
(global-set-key (kbd "M-r") 'revert-buffer)

(switch-to-buffer "*Messages*")

;; (find-file "~/.config/emacs/init-minimal.el")
;; ^base


;; https://github.com/progfolio/elpaca/issues/222#issuecomment-1872596777
;; https://github.com/NixOS/nixpkgs/issues/267548
(setq elpaca-core-date '(20231211))

;; installer
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; installer end




;; (setq elpaca-verbosity most-positive-fixnum)

;; load directly before elpaca-use-package to register :blackout use-package handler
(elpaca blackout)

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

(elpaca-wait)

;; ^base2

(elpaca-test

  (elpaca elpaca-use-package
    ;; Enable use-package :ensure support for Elpaca.
    (elpaca-use-package-mode)
    (setq use-package-always-ensure t))

  (use-package insert-shebang
    :commands insert-shebang
    :config
    (remove-hook 'find-file-hook 'insert-shebang)
    :preface
    (setq my-insert-shebang-enabled nil)
    :custom
    (insert-shebang-track-ignored-filename nil))
  )
