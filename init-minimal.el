;; emacs -q --load ~/.config/emacs/init-minimal.el


;; (setq straight-use-package-by-default t
;;       initial-scratch-message nil
;;       inhibit-startup-screen t)

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)

;; (use-package gif-screencast
;;   :commands gif-screencast-start-or-stop
;;   :custom
;;   (gif-screencast-program "grim")
;;   (gif-screencast-args ()))

;; (global-set-key (kbd "C-f") 'find-file)
;; (global-set-key (kbd "M-d") 'switch-to-buffer)
;; (global-set-key (kbd "M-r") 'revert-buffer)

;; (switch-to-buffer "*Messages*")

;; (find-file "~/.config/emacs/init-minimal.el")
;; ^base

;; (setq mode-line-mule-info (eval (car (get 'mode-line-mule-info 'standard-value))))

;; (setq
;;  mode-line-front-space " "
;;  mode-line-mule-info " "
;;  mode-line-client " "
;;  mode-line-modified " "
;;  mode-line-remote " "
;;  mode-line-frame-identification " "
;;  mode-line-buffer-identification ""
;;  mode-line-position (list "%l,%c")
;;  ;; mode-line-format '("%e"
;;  ;;                     mode-line-front-space
;;  ;;                     mode-line-mule-info
;;  ;;                     mode-line-client
;;  ;;                     mode-line-modified
;;  ;;                     mode-line-remote
;;  ;;                     mode-line-frame-identification
;;  ;;                     mode-line-buffer-identification
;;  ;;                     "   "
;;  ;;                     mode-line-position
;;  ;;                     (vc-mode vc-mode)
;;  ;;                     "  "
;;  ;;                     mode-line-modes
;;  ;;                     mode-line-misc-info
;;  ;;                     mode-line-end-spaces)
;;  )

(defun my-discard-buffer-action (buf)
  (with-current-buffer buf
    (when (file-exists-p buffer-file-name)
      (revert-buffer :ignore-auto :noconfirm))
    (set-buffer-modified-p nil)
    (kill-buffer buf)))

(add-to-list 'save-some-buffers-action-alist
             `(?r my-discard-buffer-action
                  "discard this buffer"))
;; (find-file "~/tmp/test.org")
;; (find-file "~/tmp/test.md")
