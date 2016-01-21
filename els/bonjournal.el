;;; bonjournal.el --- Write journal in emacs org-mode  -*- lexical-binding: t; -*-

(defvar bonjournal--entryDate 0)
(defvar bonjournal-dir-linux "~/bonjournal/")
(defvar bonjournal-dir-windows "~/bonjournal/")
(defvar bonjournal--day-file "day.org")
(defvar bonjournal--journal-file "bonjournal.gpg")
(defvar bonjournal-entryTemplate "\n\n/%H:%M/, *Je pense*\n")
(defvar bonjournal--entryTemplateRegex "\/[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\/, \*")



(defsubst bonjournal--defineMode ()
  (let ((keymap (make-keymap)))
    ;; (define-key keymap (kbd "C-c C-c") 'bonjournal--save-day)
    (define-minor-mode bonjournal-mode
      "bonjournal functions"
      nil "" keymap)))

(defsubst bonjournal--enableModes ()
  (bonjournal--defineMode)
  (bonjournal-mode 1)
  (diminish 'bonjournal-mode)
  (org-mode))

(defsubst bonjournal--with-safeFileVariable (&rest body)
  (setq enable-local-variables :all)
  (eval body)
  (setq enable-local-variables t))

(defsubst bonjournal--getDate ()
  (bonjournal--with-safeFileVariable
   'hack-local-variables)
  (when (boundp 'date)
    (setq bonjournal--entryDate (calendar-absolute-from-gregorian (read date)))))

(defsubst bonjournal--isNewDay ()
  (> (org-today) bonjournal--entryDate))

(defsubst bonjournal--setDate ()
  (setq bonjournal--entryDate (org-today))
  (let ((date
	 (format "%S" (calendar-gregorian-from-absolute bonjournal--entryDate))))
    (add-file-local-variable-prop-line 'date date)))

(defsubst bonjournal--hasEntry ()
  (> (count-lines (point-min) (point-max)) 1))

(defsubst bonjournal--onEntryLine ()
  (looking-at bonjournal--entryTemplateRegex))

(defsubst bonjournal--onEmptyLine ()
  (eq (point) (point-max)))

(defsubst bonjournal--cleanLine ()
  (back-to-indentation)
  (when (or (bonjournal--onEmptyLine)
	    (bonjournal--onEntryLine))
    (goto-char (line-end-position))
    (kill-line 0)
    (delete-char -1)
    t))

(defsubst bonjournal--cleanLastEntry ()
  (let ((lineCleaned t))
    (goto-char (point-max))
    (while lineCleaned
      (setq lineCleaned (bonjournal--cleanLine)))
    (goto-char (point-max))))

(defsubst bonjournal--deleteEntries ()
  (goto-char (point-min))
  (delete-region (line-beginning-position 2) (point-max)))

(defsubst bonjournal--getEntries ()
  (goto-char (point-min))
  (let ((secondLine (line-beginning-position 2)))
    (buffer-substring-no-properties secondLine (point-max))))

(defsubst bonjournal--writingFail ()
  (message "Filesize not increased. Maybe something went wrong")
  (set-window-buffer (selected-window) (current-buffer)))

(defsubst bonjournal--drivePush ()
  (message "pushing bonjournal...")
  (shell-command-to-string (concat "drive push " bonjournal--journal-file))
  (message "done"))

(defsubst bonjournal--drivePull ()
  (message "pulling bonjournal...")
  (shell-command-to-string (concat "drive pull " bonjournal--journal-file))
  (message "done"))

(defsubst bonjournal--writingSuccess ()
  (setq require-final-newline nil)
  (save-buffer)
  (bonjournal--drivePush)
  (kill-buffer))

(defmacro bonjournal--with-enablePassCache (&rest body)
  `(progn
     (setq epa-file-cache-passphrase-for-symmetric-encryption t)
     ,@body
     (setq epa-file-cache-passphrase-for-symmetric-encryption nil)))

(defsubst bonjournal--getBonjournalDir ()
  (if (eq system-type 'gnu/linux)
      bonjournal-dir-linux
    bonjournal-dir-windows))

(defsubst bonjournal--getDayPath ()
  (concat (bonjournal--getBonjournalDir) bonjournal--day-file))

(defsubst bonjournal--getJournalPath ()
  (concat (bonjournal--getBonjournalDir) bonjournal--journal-file))

(defsubst bonjournal--writeToJournal (entry)
  (bonjournal--with-enablePassCache
   (with-current-buffer (find-file (bonjournal--getJournalPath))
    (let ((oldSize (buffer-size)))
      (org-datetree-find-date-create (calendar-gregorian-from-absolute bonjournal--entryDate))
      (goto-char (point-max))
      (insert entry)
      (if (> (buffer-size) oldSize)
	  (bonjournal--writingSuccess)
	(bonjournal--writingFail))))))

(defsubst bonjournal--saveEntry ()
  (let ((entry (bonjournal--getEntries)))
    (bonjournal--deleteEntries)
    (save-buffer)
    (bonjournal--writeToJournal entry)))

(defsubst bonjournal--insertEntryTemplate ()
  (insert (format-time-string bonjournal-entryTemplate)))

(defsubst bonjournal--displayBuffer ()
  (set-window-start (selected-window) 1)
  (set-window-buffer (selected-window) (current-buffer))
  (delete-other-windows))

(defsubst bonjournal--makeBuffer ()
  (with-current-buffer (find-file (bonjournal--getDayPath))
      (bonjournal--drivePull)
      (bonjournal--enableModes)
      (when (not (bonjournal--getDate))
	(bonjournal--setDate))
      (bonjournal--cleanLastEntry)
      (when (and (bonjournal--isNewDay) (bonjournal--hasEntry))
	  (bonjournal--saveEntry))
      (bonjournal--insertEntryTemplate)
      (bonjournal--displayBuffer)))

(defun bonjournal-new-entry ()
  (interactive)
   (bonjournal--makeBuffer))

(provide 'bonjournal)
;;; bonjournal.el ends here
