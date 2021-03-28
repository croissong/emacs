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


(defhydra my-hydras--ediff (:color blue)
  ""
  ("b" ediff-buffers :column "buffers")
  ("B" ediff-buffers3 "3way" :column "buffers")
  ("f" ediff-files :column "files")
  ("F" ediff-files3 :column "3way":column "files")
  ("c" ediff-current-file "current" :column "files")
  ("r" ediff-revision "revision" :column "VC")
  ("l" ediff-regions-linewise "linewise" :column "regions")
  ("w" ediff-regions-wordwise "wordwise" :column "regions")
  ("q" my-hydras--pop :column ""))


(defhydra my-hydras-files (:color blue)
  ""
  ("e" (find-file (expand-file-name "init.org" user-emacs-directory)) "emacs" :column "config")
  ("n" (find-file (substitute-in-file-name "$DOTFILES/dot_config/nyxt/init.org")) "nyxt" :column "config"))

(defhydra my-hydras-misc (:color blue)
  ""
  ("+" (call-interactively 'text-scale-adjust) :column "zoom")
  ("-" (call-interactively 'text-scale-adjust) :column "zoom")
  ("0" (call-interactively 'text-scale-adjust) :column "zoom"))

(defhydra my-hydras-code (:color blue)
  ""
  ("f" (call-interactively 'format-all-buffer) "format" :column "edit")
  ("r" (call-interactively 'replace-regexp) "replace" :column "edit")

  ("1" (crux-rename-file-and-buffer) "rename" :column "file")
  ("a" (kill-new buffer-file-name) "absolute path" :column "file")

  ("e" (progn
         (my-hydras--ediff/body)
         (my-hydras--push '(my-hydras-code/body))) "ediff..." :column "more"))

(provide 'my-hydras)

;;; my-hydras.el ends here
