

;; newest version: für 100 2.9s
;; (benchmark-run 100 (with-current-buffer (find-file-noselect my/toutesuit-file)
;;     (let ((id (random
;; 	       (string-to-number (org-entry-get (end-of-buffer) "ID"))))
;; 	  (counter 1)
;; 	  toutesuit)
;;       (catch 'counter
;; 	(org-map-entries '(lambda ()
;; 			    (when (= counter id)
;; 				(setq toutesuit
;; 				      (cons (nth 4 (org-heading-components))
;; 					    (org-entry-get (point) "URL")))
;; 				(funcall interprogram-cut-function (cdr toutesuit))
;; 				(throw 'counter counter))
;; 			    (setq counter (1+ counter)))))
;;       )))

;; new version: für 100 4.45s
;; (benchmark-run 100 (with-current-buffer (find-file-noselect my/toutesuit-file)
;;     (let ((id (random
;; 	       (string-to-number (org-entry-get (end-of-buffer) "ID"))))
;; 	  (counter 1)
;; 	  toutesuit)
;;       (while (<= counter id)
;; 	(org-map-entries '(lambda ()
;; 			    (when (= counter id)
;; 				(setq toutesuit
;; 				      (cons (nth 4 (org-heading-components))
;; 					    (org-entry-get (point) "URL")))
;; 				(funcall interprogram-cut-function (cdr toutesuit)))
;; 			    (setq counter (1+ counter))
;;       ;; (minibuffer-message (car toutesuit))
;; 			    ))))))

;; previous version: 0.176sec
;; (benchmark-run (with-current-buffer (find-file-noselect my/toutesuit-file)
;;     (let (urls toutesuit)
;;       (org-map-entries '(lambda ()
;; 			  (push (cons (nth 4 (org-heading-components)) (org-entry-get (point) "URL")) urls))
;; 		       "LEVEL>1")
;;       (setq toutesuit (nth (random (length urls)) urls))
;;       (funcall interprogram-cut-function (cdr toutesuit))
;;       )))

;; (defun my/change-toutesuit ()
;;   (interactive)
;;   (with-current-buffer (find-file-noselect my/toutesuit-file)
;;     (let ((id 1))
;; 	(org-map-entries '(lambda ()
;; 			    (org-set-property "ID" (number-to-string id))
;; 			    (setq id (1+ id)))
;; 			    "LEVEL>1")
;; 	)))

