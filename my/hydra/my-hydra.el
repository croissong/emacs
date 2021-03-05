;;; my-hydras.el --- All my hydras  -*- lexical-binding: t; -*-

(require 'hydra)

(defvar hydra-stack nil)

(defun hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x
      (funcall x))))

(defhydra hydra-my/init (:color teal)
  "my init"
  ("s" (switch-to-buffer "*scratch*") "scratch")
  ("d" (magit-status-internal "~/dotfiles")
       "dotfiles")
  ("c" (progn
         (hydra-my/code/body)
         (hydra-push '(hydra-my/init/body)))
       "code")
  ("i" (org-init-open)
       "org-init")
  ("t" (progn
         (hydra-my/tmux/body)
         (hydra-push '(hydra-my/init/body)))
       "tmux")
  ("q" hydra-pop "exit"))

(defhydra hydra-my/code (:color teal)
  "code"
  ("m" (let ((magit-repository-directories '(("~/code/my" . 1))))
              (magit-list-repositories))
       "my")
  ("w" (let ((magit-repository-directories '(("~/code/wrk" . 2))))
              (magit-list-repositories))
       "wrk")
  ("s" (let ((magit-repository-directories '(("~/code/svh" . 3))))
              (magit-list-repositories))
       "svh")
  ("q" hydra-pop "exit"))

(defhydra hydra-my/tmux (:color teal)
  "tmux"
  ("y" (progn
         (pop-to-buffer "*tmux*")
         (emamux:set-parameters)
         (emamux:tmux-run-command t "capture-pane" "-p" "-S" "-50" "-t" (emamux:target-session))
         ))
  ("q" hydra-pop "exit")
  )

(provide 'my-hydra)

;; TODO C-x C-+ for zoom
;;; my-hydras.el ends here
