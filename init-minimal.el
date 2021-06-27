;; emacs -q --load ~/.config/emacs/init-minimal.el


(setq straight-use-package-by-default t
      use-package-compute-statistics nil
      comp-async-report-warnings-errors nil
      straight-check-for-modifications '(find-when-checking)

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

(use-package a)
(use-package ctrlf
  :init (ctrlf-mode +1)
  (message "hooo")
    :config
    (setq ctrlf-minibuffer-bindings (a-assoc ctrlf-minibuffer-bindings
                                        "M-ä" 'ctrlf-next-match
                                        "M-ü" 'ctrlf-previous-match
                                        "C-r" 'ctrlf-change-search-style)))




(switch-to-buffer "*Messages*")
