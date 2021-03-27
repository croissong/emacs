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


(defhydra my-hydras-files (:color teal)
  "
^----^--
 _e_: emacs
 _n_: nyxt
 ^----^
"
  ("e" (find-file (expand-file-name "init.org" user-emacs-directory)))
  ("n" (find-file (substitute-in-file-name "$DOTFILES/dot_config/nyxt/init.org")))
  ("q" my-hydras--pop "exit"))

(defhydra my-hydras-misc (:color teal :hint nil)
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
 _r_: replace-regexp
 _a_: copy-absolute-filename
 _1_: rename-file
 ^----^
"
  ("f" (call-interactively 'format-all-buffer))
  ("r" (call-interactively 'replace-regexp))
  ("a" (kill-new buffer-file-name))
  ("1" (crux-rename-file-and-buffer))
  ("q" my-hydras--pop "exit"))

(provide 'my-hydras)

;;; my-hydras.el ends here
