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



(use-package ebuku
  :load-path "elpaca/repos/ebuku/"
  :commands ebuku)


;; (setq mode-line-mule-info (eval (car (get 'mode-line-mule-info 'standard-value))))



(setq-default
 mode-line-position (list "%l,%c")
 mode-line-format '("%e" mode-line-front-space
                    (:propertize
                     ("" mode-line-mule-info)
                     display (min-width (2.0)))
                    mode-line-frame-identification mode-line-buffer-identification "  " mode-line-position
                    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
 )

(message "hi")


;; (defun my-discard-buffer-action (buf)
;;   (with-current-buffer buf
;;     (when (file-exists-p buffer-file-name)
;;       (revert-buffer :ignore-auto :noconfirm))
;;     (set-buffer-modified-p nil)
;;     (kill-buffer buf)))

;; (add-to-list 'save-some-buffers-action-alist
;;              `(?r my-discard-buffer-action
;;                   "discard this buffer"))
;; (find-file "~/tmp/test.org")
;; (find-file "~/tmp/test.md")
