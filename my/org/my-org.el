;;; my-org.el --- My customizations for org-mode  -*- lexical-binding: t; -*-

(defun my-org-sentence-newline()
  (interactive)
  (org-backward-sentence)
  (org-delete-backward-char 1)
  (org-return-indent))

(defun my-org-delete-heading-or-line ()
  (interactive)
  (if (org-at-heading-p)
      (org-cut-subtree)
    (kill-line)))

(provide 'my-org)

;;; my-org.el ends here
