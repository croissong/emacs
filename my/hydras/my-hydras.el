;;; my-hydras.el --- All my hydras  -*- lexical-binding: t; -*-

(require 'hydra)

(defvar my-hydras--stack nil)

(defun my-hydras--push (expr)
  (push `(lambda () ,expr) my-hydras--stack))

(defun my-hydras--pop ()
  (interactive)
  (let ((x (pop my-hydras--stack)))
    (when x
      (funcall x))))


(defhydra my-hydras-init (:color teal)
  "file"
  ("s" (switch-to-buffer "*scratch*") "scratch")
  ("d" (magit-status-internal "~/dotfiles")
   "dotfiles")
  ("c" (progn
         (hydra-my/code/body)
         (my-hydras--push '(hydra-my/init/body)))
   "code")
  ("i" (org-init-open)
   "org-init")
  ("t" (progn
         (hydra-my/tmux/body)
         (my-hydras--push '(hydra-my/init/body)))
   "tmux")
  ("q" my-hydras--pop "exit"))

(defhydra my-hydras-utils (:color teal :hint nil)
  "
 ^Zoom^
 ^----^--
 _+_: zoom-in
 _-_: zoom-out
 _0_: zoom-reset
 ^----^
"
  ("+" (call-interactively 'text-scale-adjust))
  ("-" (call-interactively 'text-scale-adjust))
  ("0" (call-interactively 'text-scale-adjust))
  ("q" my-hydras--pop "exit"))

(defhydra my-hydras-code (:color teal :hint nil)
  "
 ^----^--
 _f_: format-all-buffer
 _r_: replace-string
 ^----^
"
  ("f" (call-interactively 'format-all-buffer))
  ("r" (call-interactively 'replace-string))
  ("q" my-hydras--pop "exit"))



(provide 'my-hydras)

;; TODO C-x C-+ for zoom
;; google translate
;; :bind* ("C-f f" . format-all-buffer) (defalias?)

;;; my-hydras.el ends here
