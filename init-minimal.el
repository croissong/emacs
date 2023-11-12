;; emacs -q --load ~/.config/emacs/init-minimal.el


;; (setq straight-use-package-by-default t
;;       initial-scratch-message nil
;;       inhibit-startup-screen t)

;; installer
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
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
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))


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
