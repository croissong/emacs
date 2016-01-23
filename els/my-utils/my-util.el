;;; my-util.el --- Write journal in emacs org-mode  -*- lexical-binding: t; -*-

(defun my-util-linux? ()
  (eq system-type 'gnu/linux))

(defun my-util-win? ()
  (eq system-type 'windows-nt))

(defun my-util-cb? ()
  (file-exists-p "/etc/crouton/name"))

(defun my-util-ensureEmacsDir (path)
  (unless (file-directory-p (concat user-emacs-directory path))
    (mkdir (concat user-emacs-directory path) t)))

(defmacro my-util-ifInstalled (exe &rest body)
  `(if (executable-find ,exe)
       ,@body
     (message "%s not found found; please install" ,exe)))

(provide 'my-util)

;;; my-util.el ends here
