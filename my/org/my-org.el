;;; my-org.el --- My customizations for org-mode  -*- lexical-binding: t; -*-

(defun my-org-sentence-newline ()
  (interactive)
  (org-backward-sentence)
  (org-delete-backward-char 1)
  (org-return-indent))

(defun my-org-delete-heading-or-line ()
  (interactive)
  (if (org-at-heading-p)
      (org-cut-subtree)
    (kill-line)))


(defun template-key-equal-p (new-template existing-template)
  "Compare NEW-TEMPLATE and EXISTING-TEMPLATE based on their keys."
  (equal (car new-template) (car existing-template)))

(cl-pushnew
 '("i"
   "Inbox"
   entry
   (file my-tabs-inbox-file)
   "* %?"
   :hook (lambda () (call-interactively 'org-web-tools-insert-link-for-url))
   :prepend t
   :empty-lines 1
   :immediate-finish t
   :jump-to-captured t)

 org-capture-templates

 :test 'template-key-equal-p)


(provide 'my-org)

;;; my-org.el ends here
