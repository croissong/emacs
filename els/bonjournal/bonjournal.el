;;; bonjournal.el --- Write journal in emacs org-mode  -*- lexical-binding: t; -*-

(require 'org-datetree)

(defvar bonjournal--entryDate 0)
(defvar bonjournal-dir "~/bonjournal/")
(defvar bonjournal--day-file "day.org")
(defvar bonjournal--journal-file "bonjournal.gpg")
(defvar bonjournal-entryTemplate "\n\n/%H:%M/, *Je pense*\n")
(defvar bonjournal--entryTemplateRegex "\/[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\/, \*")



(defun bonjournal--defineMode ()
  (let ((keymap (make-keymap)))
    ;; (define-key keymap (kbd "C-c C-c") 'bonjournal--save-day)
    (define-minor-mode bonjournal-mode
      "bonjournal functions"
      nil "" keymap)))

(defun bonjournal--enableModes ()
  (bonjournal--defineMode)
  (bonjournal-mode 1)
  (diminish 'bonjournal-mode)
  (org-mode))

(defun bonjournal--with-safeFileVariable (&rest body)
  (setq enable-local-variables :all)
  (eval body)
  (setq enable-local-variables t))

(defun bonjournal--getDate ()
  (hack-local-variables)
  (when (boundp 'date)
    (setq bonjournal--entryDate (calendar-absolute-from-gregorian (read date)))))

(defun bonjournal--isNewDay ()
  (> (org-today) bonjournal--entryDate))

(defun bonjournal--setDate ()
  (setq bonjournal--entryDate (org-today))
  (let ((date
	 (format "%S" (calendar-gregorian-from-absolute bonjournal--entryDate))))
    (add-file-local-variable-prop-line 'date date)))

(defun bonjournal--hasEntry ()
  (> (count-lines (point-min) (point-max)) 1))

(defun bonjournal--onEntryLine ()
  (looking-at bonjournal--entryTemplateRegex))

(defun bonjournal--onEmptyLine ()
  (eq (point) (point-max)))

(defun bonjournal--cleanLine ()
  (back-to-indentation)
  (when (or (bonjournal--onEmptyLine)
	    (bonjournal--onEntryLine))
    (goto-char (line-end-position))
    (kill-line 0)
    (delete-char -1)
    t))

(defun bonjournal--cleanLastEntry ()
  (let ((lineCleaned t))
    (goto-char (point-max))
    (while lineCleaned
      (setq lineCleaned (bonjournal--cleanLine)))
    (goto-char (point-max))))

(defun bonjournal--deleteEntries ()
  (goto-char (point-min))
  (delete-region (line-beginning-position 2) (point-max)))

(defun bonjournal--getEntries ()
  (goto-char (point-min))
  (let ((secondLine (line-beginning-position 2)))
    (buffer-substring-no-properties secondLine (point-max))))

(defun bonjournal--writingFail ()
  (message "Filesize not increased. Maybe something went wrong")
  (set-window-buffer (selected-window) (current-buffer))
  nil)

(defun bonjournal--drivePush ()
  (let ((default-directory "~/cloud/dokumente/meinAll/monument/bonjournal/"))
    (message "pushing bonjournal...")
    (shell-command-to-string (concat "drive push "
				     bonjournal--journal-file
				     " " bonjournal--day-file))
    (message "done")))

(defun bonjournal--drivePull ()
  (let ((default-directory "~/cloud/dokumente/meinAll/monument/bonjournal/"))
    (message "pulling bonjournal...")
    (shell-command-to-string (concat "drive push "
				     bonjournal--journal-file
				     " " bonjournal--day-file))
    (message "done")))

(defun bonjournal--writingSuccess ()
  (setq require-final-newline nil)
  (save-buffer)
  (bonjournal--drivePush)
  (kill-buffer))

(defmacro bonjournal--with-enablePassCache (&rest body)
  `(progn
     (setq epa-file-cache-passphrase-for-symmetric-encryption t)
     ,@body
     (setq epa-file-cache-passphrase-for-symmetric-encryption nil)))

(defun bonjournal--getDayPath ()
  (concat bonjournal-dir bonjournal--day-file))

(defun bonjournal--getJournalPath ()
  (concat bonjournal-dir bonjournal--journal-file))

(defun bonjournal--insertEntry (entry date)
  (org-mode)
  (org-datetree-find-date-create (calendar-gregorian-from-absolute date))
  (goto-char (point-max))
  (insert entry))

(defun bonjournal--writeToJournal (entry)
  (bonjournal--with-enablePassCache
   (with-current-buffer (find-file (bonjournal--getJournalPath))
     (let ((oldSize (buffer-size)))
       (bonjournal--insertEntry entry bonjournal--entryDate)
       (if (> (buffer-size) oldSize)
	   (bonjournal--writingSuccess)
	 (bonjournal--writingFail))))))

(defun bonjournal--saveEntry ()
  (let ((entry (bonjournal--getEntries)))
    (when (bonjournal--writeToJournal entry)
	(progn
	  (bonjournal--deleteEntries)
	  (save-buffer)))))

(defun bonjournal--insertEntryTemplate ()
  (goto-char (point-max))
  (insert (format-time-string bonjournal-entryTemplate)))

(defun bonjournal--displayBuffer ()
  (set-window-start (selected-window) 1)
  (set-window-buffer (selected-window) (current-buffer))
  (delete-other-windows))

(defun bonjournal--makeBuffer ()
  (with-current-buffer (find-file (bonjournal--getDayPath))
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
  (bonjournal--drivePull)
  (bonjournal--with-safeFileVariable
   'bonjournal--makeBuffer))

(defun bonjournal--parseNextHash ()
  (search-forward "#")
  (let ((hash bonjournal--getHashAtPoint)
	(content bonjournal--getHashContent))
    (bonjournal--processHash hash content)
  (bonjournal--parseNextHash)))

(defun bonjournal--getHashAtPoint ()
  (let ((hash (thing-at-point 'sexp t)))
    (bonjournal--removeHash)
    (setq hash (replace-regexp-in-string "#" "" hash))
    hash))

(defun bonjournal--getHashContent ()
  (bonjournal--getHashSentence))

(defun bonjournal--getHashSentence ()
  (thing-at-point 'sentence))

(defun bonjournal--processHash (hash content)
  (let ((hashGroups (bonjournal--getHashGroups hash)))
    (bonjournal--createHashEntry hash content hashgroups)))

(defun bonjournal--createHashEntry (hash content groups)
  (cond (bonjournal--isTodo groups))
  )

(defun bonjournal--getHashGroups (hash)
  )

(defun bonjournal--isTodo (groups)
  (eq (last groups) "todo"))

(defun bonjournal--removeHash ()
   (let ((hashBounds (bounds-of-thing-at-point 'sexp)))
  (delete-region (car hashBounds) (cdr hashBounds))))

(defun bonjournal--parseHashes ()
  (bonjournal--parseNextHash))

(defun bonjournal--anaylizeEntry (entry)
  (with-temp-buffer
    (insert entry)
    (goto-char (point-min))
    (bonjournal--parseHashes)))

(provide 'bonjournal)
;;; bonjournal.el ends here

