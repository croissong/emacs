;;; my-menus.el --- custom transient menus -*- lexical-binding: t; -*-



;; TODO SNIPPETS:
;; dired-mode: W binding to open file natively
;; magit expand commit file diff to full commit
;; magit-diff-toggle-file-filter, by default bound to DF.

(require 'transient)

(transient-define-prefix
 my-menus-files ()
 [["config" ("e" "emacs"
    (lambda ()
      (interactive)
      (find-file (expand-file-name "init.org" user-emacs-directory))))
   ("s" "system"
    (lambda ()
      (interactive)
      (let ((project-current-directory-override
             (f-expand "system/" (substitute-env-vars "$DOT"))))
        (consult-project-extra-find))))

   ("d" "dotfiles"
    (lambda ()
      (interactive)
      (let ((project-current-directory-override
             (f-expand "dotfiles/" (substitute-env-vars "$DOT"))))
        (consult-project-extra-find))))
   ("j" "justfile"
    (lambda ()
      (interactive)
      (find-file
       (expand-file-name "dotfiles/dot_config/just/justfile"
                         (substitute-env-vars "$DOT")))))
   ("g" "grm" my-menus--files-grm)
   ("n" "nyxt"
    (lambda ()
      (interactive)
      (find-file (expand-file-name "init.org" user-emacs-directory))))]
  ["moi" ("h" "hieroglyph"
    (lambda ()
      (interactive)
      (find-file "~/hieroglyph/rashid.yaml")))

   ("t" "timelog"
    (lambda ()
      (interactive)
      (find-file
       (my-menus--latest-file (expand-file-name "docs/wrk/timelog/"
                                                (substitute-env-vars "$DOT"))
                              "klg"))))

   ("p" "priv"
    (lambda ()
      (interactive)
      (let ((project-current-directory-override
             (f-expand "priv/" (substitute-env-vars "$DOT"))))
        (consult-project-extra-find))))]])


(transient-define-prefix
 my-menus--files-grm ()
 ["grm" ("m" "moi"
   (lambda ()
     (interactive)
     (find-file
      (expand-file-name "dotfiles/dot_config/git-repo-manager/config.yaml.tmpl"
                        (substitute-env-vars "$DOT")))))
  ("w" "wrk"
   (lambda ()
     (interactive)
     (find-file
      (expand-file-name "priv/dot/git-repo-manager/wrk.yaml"
                        (substitute-env-vars "$DOT")))))])


(transient-define-prefix
 lsp ()
 ["lsp" ("r" "references"
   (lambda ()
     (interactive)
     (xref-find-references)))])

(transient-define-prefix
 my-menus-consult ()
 ["consult" ("f" "flymake" consult-flymake) ("m" "imenu" consult-imenu)])

(transient-define-prefix
 my-menus-code ()
 [["edit"
   ("e" "eldoc-info" eldoc-print-current-symbol-info)
   ("r" "search-replace" isearch-forward-regexp)
   ("i" "string-inflection-all-cycle" string-inflection-all-cycle :transient t)]

  ["file"

   ("1" "rename" rename-visited-file)

   ("2" "delete" my-snippets-delete-file-and-buffer)

   ("3" "copy" write-file)]
  ["" ("f" "file" my-snippets-copy-buffer-path)
   ("C-f" "file abs"
    (lambda ()
      (interactive)
      (my-snippets-copy-buffer-path t)))
   ("d" "dir" my-snippets-copy-buffer-dir)
   ("C-d" "dir abs"
    (lambda ()
      (interactive)
      (my-snippets-copy-buffer-dir t)))]

  ["misc"

   ("+" "zoom" text-scale-adjust)]])

(defun my-menus--copy-file-dir ()
  (interactive)
  (let ((absolute
         (transient-arg-value
          "abs" (transient-args transient-current-command))))
    (my-snippets-copy-buffer-dir absolute)))

(defun my-menus--latest-file (path &optional match)
  "Get latest file (including directory) in PATH."
  (car (directory-files path 'full (or match nil) #'file-newer-than-file-p)))

(provide 'my-menus)

;;; my-menus.el ends here
