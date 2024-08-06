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
   ("p" "packages"
    (lambda ()
      (interactive)
      (find-file
       (expand-file-name "system/nix-config/hm/pkgs.nix"
                         (substitute-env-vars "$DOT")))))
   ("d" "dotfiles"
    (lambda ()
      (interactive)
      (let ((default-directory
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
       (expand-file-name "docs/wrk/timelog/" (substitute-env-vars "$DOT")))))]])

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
   ("i" "string-inflection-all-cycle" string-inflection-all-cycle)]

  ["file"

   ("1" "rename" rename-visited-file)

   ("2" "delete" my-snippets-delete-file-and-buffer)

   ("3" "copy" write-file)

   ("a" "path"
    (lambda (absolute-p)
      (interactive "P")
      (my-snippets-copy-buffer-path absolute-p)))]

  ["misc"

   ("+" "zoom" text-scale-adjust)]])

(provide 'my-menus)

;;; my-menus.el ends here
