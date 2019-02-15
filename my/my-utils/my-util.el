;;; my-util.el --- Write journal in emacs org-mode  -*- lexical-binding: t; -*-

(defun my-util-win? ()
  (eq system-type 'windows-nt))

(defun my-util-ensure-emacs-dir (path)
  (let ((expanded-path (expand-file-name path user-emacs-directory )))
    (unless (file-directory-p expanded-path)
      (mkdir expanded-path t))
    expanded-path))

(defun my-util-installed? (exe)
  (unless (executable-find exe)
    (message "%s not found found; please install" exe)
    nil))

(defmacro my-util-with-eval-after-frame (&rest body)
  `(if (daemonp)
      (add-hook 'after-make-frame-functions
                (function (lambda (frame)
                  (select-frame frame)
                  ,@body)))
      ,@body))

(provide 'my-util)

;;; my-util.el ends here
