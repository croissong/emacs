;;; toutsuit.el --- Write journal in emacs org-mode  -*- lexical-binding: t; -*-

(defvar toutsuit-file )

(defun toutsuit-getRandom ()
  (interactive)
  (with-current-buffer (find-file-noselect my/toutesuit-file)
    (let ((id (random
	       (string-to-number (org-entry-get (goto-char (point-max)) "ID"))))
	  (counter 1)
	  toutesuit point)
      (catch 'counter
	(org-map-entries '(lambda ()
			    (when (= counter id)
			      (setq point (point))
			      (setq toutesuit
				    (cons (nth 4 (org-heading-components))
					  (org-entry-get point "URL")))
			      (funcall interprogram-cut-function (cdr toutesuit))
			      (minibuffer-message (car toutesuit))
			      (throw 'counter counter))
			    (setq counter (1+ counter)))
			 "LEVEL>1"))
      (goto-char point)
      (org-set-tags)
      (save-buffer)
      (kill-buffer))))

(provide 'toutesuit)

;;; toutesuit.el ends here
