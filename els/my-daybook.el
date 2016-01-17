;;; my-daybook.el --- stuff  -*- lexical-binding: t; -*-

(defvar my-daybook--entryDate 0)
(defvar my-daybook-dir)
(defvar my-daybook-day-file "~/Downloads/test.org")
(defvar my-daybook-daybook-file "~/Downloads/testDay.gpg")
(defvar my-daybook-entryTemplate "\n\n/%H:%M/, *Je pense*\n")
(defvar my-daybook--entryTemplateRegex "\/[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\/, \*")



(defsubst my-daybook--defineMode ()
  (let ((keymap (make-keymap)))
    ;; (define-key keymap (kbd "C-c C-c") 'my-daybook--save-day)
    (define-minor-mode my-daybook-mode
      "daybook functions"
      nil "" keymap)))

(defsubst my-daybook--enableModes ()
  (my-daybook--defineMode)
  (my-daybook-mode 1)
  (diminish 'my-daybook-mode)
  (org-mode))

(defsubst my-daybook--with-safeFileVariable (&rest body)
  (setq enable-local-variables :all)
  (eval body)
  (setq enable-local-variables t))

(defsubst my-daybook--getDate ()
  (my-daybook--with-safeFileVariable
   'hack-local-variables)
  (when (boundp 'date)
    (setq my-daybook--entryDate (calendar-absolute-from-gregorian (read date)))))

(defsubst my-daybook--newDay ()
  (> (org-today) my-daybook--entryDate))

(defsubst my-daybook--setDate ()
  (setq my-daybook--entryDate (org-today))
  (let ((date
	 (format "%S" (calendar-gregorian-from-absolute my-daybook--entryDate))))
    (add-file-local-variable-prop-line 'date date)))

(defsubst my-daybook--hasEntry ()
  (> (count-lines (point-min) (point-max)) 1))

(defsubst my-daybook--onEntryLine ()
  (looking-at my-daybook--entryTemplateRegex))

(defsubst my-daybook--onEmptyLine ()
  (eq (point) (point-max)))

(defsubst my-daybook--cleanLine ()
  (back-to-indentation)
  (when (or (my-daybook--onEmptyLine)
	    (my-daybook--onEntryLine))
    (goto-char (line-end-position))
    (kill-line 0)
    (delete-char -1)
    t))

(defsubst my-daybook--cleanLastEntry ()
  (let ((lineCleaned t))
    (goto-char (point-max))
    (while lineCleaned
      (setq lineCleaned (my-daybook--cleanLine)))
    (goto-char (point-max))))

(defsubst my-daybook--deleteEntries ()
  (goto-char (point-min))
  (delete-region (line-beginning-position 2) (point-max)))

(defsubst my-daybook--getEntries ()
  (goto-char (point-min))
  (let ((secondLine (line-beginning-position 2)))
    (buffer-substring-no-properties secondLine (point-max))))

(defsubst my-daybook--writingFail ()
  (message "Filesize not increased. Maybe something went wrong")
  (set-window-buffer (selected-window) (current-buffer)))

(defun my-daybook--pushToGdrive ()
  ;; TODO 
  )

(defsubst my-daybook--writingSuccess ()
  (setq require-final-newline nil)
  (save-buffer)
  (kill-buffer)
  (my-daybook--pushToGdrive))

(defmacro my-daybook--with-enablePassCache (&rest body)
  `(progn
     (setq epa-file-cache-passphrase-for-symmetric-encryption t)
     ,@body
     (setq epa-file-cache-passphrase-for-symmetric-encryption nil)))

(defsubst my-daybook--writeToDaybook (entry)
  (my-daybook--with-enablePassCache
   (with-current-buffer (find-file my-daybook-daybook-file)
    (let ((oldSize (buffer-size)))
      (org-datetree-find-date-create (calendar-gregorian-from-absolute my-daybook--entryDate))
      (goto-char (point-max))
      (insert entry)
      (if (> (buffer-size) oldSize)
	  (my-daybook--writingSuccess)
	(my-daybook--writingFail))))))

(defsubst my-daybook--saveEntry ()
  (let ((entry (my-daybook--getEntries)))
    (my-daybook--deleteEntries)
    (save-buffer)
    (my-daybook--writeToDaybook entry)))

(defsubst my-daybook--insertEntryTemplate ()
  (insert (format-time-string my-daybook-entryTemplate)))

(defsubst my-daybook--displayBuffer ()
  (set-window-start (selected-window) 1)
  (set-window-buffer (selected-window) (current-buffer))
  (delete-other-windows))

(defsubst my-daybook--makeBuffer ()
  (let ((day-file (concat my-daybook-dir "day.org")))
    (with-current-buffer (find-file my-daybook-day-file)
      (my-daybook--enableModes)
      (when (not (my-daybook--getDate))
	(my-daybook--setDate))
      (my-daybook--cleanLastEntry)
      (when (and (my-daybook--newDay) (my-daybook--hasEntry))
	  (my-daybook--saveEntry))
      (my-daybook--insertEntryTemplate)
      (my-daybook--displayBuffer))))

(defun my-daybook-new-entry ()
  (interactive)
   (my-daybook--makeBuffer))

(provide 'my-daybook)
;;; my-daybook.el ends here
