;; emacs -q --load ~/.config/emacs/init-minimal.el


(setq straight-use-package-by-default t
      initial-scratch-message nil
      inhibit-startup-screen t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package gif-screencast
  :commands gif-screencast-start-or-stop
  :custom
  (gif-screencast-program "grim")
  (gif-screencast-args ()))

(global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "M-d") 'switch-to-buffer)

(switch-to-buffer "*Messages*")
