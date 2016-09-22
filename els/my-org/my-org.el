;;; my-org.el --- Write journal in emacs org-mode  -*- lexical-binding: t; -*-

(defun my-org-getRandom ()
  (interactive)
  (with-current-buffer (find-file-noselect my/my-org-file)
    (let 
      (catch 'counter
	(org-map-entries '(lambda ()
			    (when (= counter id)
			      (setq point (point))
			      (setq my-org
				    (cons (nth 4 (org-heading-components))
					  (org-entry-get point "URL")))
			      (funcall interprogram-cut-function (cdr my-org))
			      (minibuffer-message (car my-org))
			      (throw 'counter counter))
			    (setq counter (1+ counter)))
			 "LEVEL>1"))
      (goto-char point)
      (org-set-tags)
      (save-buffer)
      (kill-buffer))))

(defun my-org-getRandomEntry (pMin pMax level)
  (my-org--gotoRndPos pMin pMax)
  (my-org--getCurrentLvl))


(defsubst my-org--getCurrentLvl ()
  )


(defsubst my-org--gotoRndPos (pMin pMax)
  (let ((rndPos (+ pMin (random pMax))))
    (goto-char rndPos)))

(defun my-org-export-all ()
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Note that subtrees must have the :EXPORT_FILE_NAME: property set
to a unique value for this to work properly."
  (interactive) 
  (org-map-entries (lambda ()
                     (let* ((heading (nth 4 (org-heading-components)))
                            (title (format "<!-- title: %s -->" heading))
                            (file (org-entry-get (point) "EXPORT_FILE_NAME"))) 
                       (with-current-buffer (org-html-export-as-html nil t nil t)
                         (insert title)
                         (newline)
                         (write-file file)
                         (kill-buffer))
                       )) "-noexport"))
(provide 'my-org)

;;; my-org.el ends here
