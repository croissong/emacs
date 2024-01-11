;;; my-menus.el --- custom transient menus -*- lexical-binding: t; -*-

(transient-define-prefix my-menus-files ()
  [
   [
    "config"
    ("e" "emacs"
     (lambda () (interactive)
       (find-file (expand-file-name "init.org" user-emacs-directory))
       ))
    ("p" "packages"
     (lambda ()
       (interactive)
       (find-file (expand-file-name "dot_config/nixpkgs/packages.nix" (substitute-env-vars "$DOT")))
       ))
    ("n" "nyxt"
     (lambda ()
       (interactive)
       (find-file (expand-file-name "init.org" user-emacs-directory))
       ))
    ]
   [
    "moi"
    ("h" "hieroglyph"
     (lambda ()
       (interactive)
       (find-file "~/hieroglyph/rashid.yaml")
       ))

    ("t" "timelog"
     (lambda ()
       (interactive)
       (find-file "~/Docs/wrk/timelog")
       ))
    ]
   ]
  )


(transient-define-prefix lsp ()
  [ "lsp"
    ("r" "references"
     (lambda ()
       (interactive)
       (xref-find-references)
       ))
    ]
  )

(transient-define-prefix my-menus-code ()
  [
   ["edit"

    ("r" "search-replace"
     isearch-forward-regexp)
    ]

   ["file"

    ("1" "rename"
     rename-visited-file)

    ("2" "delete"
     my-snippets-delete-file-and-buffer)

    ("3" "copy"
     write-file)

    ("a" "path"
     (lambda (absolute-p)
       (interactive "P")
       (my-snippets-kill-buffer-path absolute-p)))
    ]

   ["misc"

    ("+" "zoom" text-scale-adjust)
    ]
   ]
  )

;;; my-menus.el ends here
