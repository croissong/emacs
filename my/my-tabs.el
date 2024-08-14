;;; my-tabs.el ---  -*- lexical-binding: t; -*-

(require 'transient)

(defvar-keymap my-tabs-mode-map
  "C-M-e"
  #'my-tabs-menu)

(define-minor-mode my-tabs-mode
  "Toggle `my-tabs-mode`."
  :keymap my-tabs-mode-map

  (my-tabs-menu))

(defcustom my-tabs-cleanup-list-file "~/dot/priv/tabs-cleanup-list.txt"
  "")
(defcustom my-tabs-dump-file-moi "~/dot/tabs/dump.org"
  "")
(defcustom my-tabs-dump-file-wrk "~/dot/tabs/dump-wrk.org"
  "")


(transient-define-prefix
 my-tabs-menu ()
 [["all" ("c" "cleanup" my-tabs-cleanup-tabs)]

  ["tab"
   ("a" "activate" my-tabs-activate-tab :transient t)
   ("d" "delete matching" my-tabs-delete-matching)]

  ["mv"
   ("m" "moi" my-tabs-move-tab-to-dump-moi)
   ("w" "wrk" my-tabs-move-tab-to-dump-wrk)]])

(defun my-tabs ()
  (interactive)
  (async-shell-command "brotab move")
  (sleep-for 0.5)
  (let ((brotab-buffer (car (buffer-list))))
    (with-current-buffer brotab-buffer
      (delete-other-windows)
      (my-tabs-mode))))

(defun my-tabs-cleanup-tabs ()
  (interactive)
  (let ((lines (split-string (buffer-string) "\n" t))
        (cleanup-list-regex (my-tabs--read-cleanup-list-regex))
        line-count)

    (setq line-count (length lines))
    (setq lines (mapcar #'my-tabs--parse-line lines))

    ;; remove lines matching `cleanup-list-regex` from parsed lines
    (setq lines
          (cl-remove-if
           (lambda (line)
             (string-match-p cleanup-list-regex (plist-get line :url)))
           lines))
    ;; deduple & sort lines based on url
    (setq lines
          (cl-remove-duplicates
           lines
           :test #'equal
           :key #'(lambda (plist) (plist-get plist :url))))

    ;; TODO: fix brotab move error
    ;; (setq lines (sort lines (lambda (a b)
    ;;                           (string-lessp (plist-get a :url) (plist-get b :url)))))


    (erase-buffer)
    (dolist (line lines)
      (insert
       (format "%s\t%s\t%s\n"
               (plist-get line :id)
               (plist-get line :title)
               (plist-get line :url))))

    (setq new-line-count (length lines))
    (message "Cleaned up %s tabs" (- line-count new-line-count)))
  (goto-char (point-min)))

(defun my-tabs-activate-tab ()
  (interactive)
  (let* ((line
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))
         (id (plist-get (my-tabs--parse-line line) :id)))
    (my-tabs--brotab-command (format "activate %s" id))))

(defun my-tabs-delete-matching (start end)
  (interactive "r")
  (let ((search-upper-case nil)
        (search-term (current-kill 0 t))
        deleted-line-count)
    (when-let* (((use-region-p))
                (region (buffer-substring start end)))
      ;; use region as search-term if active
      (setq search-term region))
    (save-excursion
      (beginning-of-buffer)
      (setq deleted-line-count (delete-matching-lines search-term))
      (message "Removed %s tabs matching %s" deleted-line-count search-term))))

(defun my-tabs-move-tab-to-dump-moi ()
  (interactive)
  (my-tabs--move-tab-to-dump my-tabs-dump-file-moi))

(defun my-tabs-move-tab-to-dump-wrk ()
  (interactive)
  (my-tabs--move-tab-to-dump my-tabs-dump-file-wrk))

(defun my-tabs--move-tab-to-dump (dump-file)
  (let* ((line
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))
         (parts (split-string line "\t" t))
         (title (nth 1 parts))
         (url (nth 2 parts)))
    (with-current-buffer (find-file-noselect dump-file)
      (my-tabs--insert-dump-line title url)
      (display-buffer (current-buffer)
                      '(display-buffer-in-direction . '(direction . right))))
    (delete-line)))

(defun my-tabs--insert-dump-line (title url)
  "Insert TITLE and URL into DUMP-FILE if URL does not already exist."
  (goto-char (point-min))
  (let* ((regex (regexp-quote (format "[[%s]" url)))
         (existing-line (re-search-forward regex nil t)))
    (if existing-line
        (message "Link already exist in line %s" (line-number-at-pos))
      (goto-char (point-max))
      (insert (format "- [[%s][%s]]\n" url title)))))

(defun my-tabs--read-cleanup-list-regex ()
  (with-temp-buffer
    (insert-file-contents my-tabs-cleanup-list-file)
    (let ((blacklist (split-string (buffer-string) "\n" t)))
      (mapconcat
       (lambda (entry) (s-replace-all '(("." . "\\.")) entry)) blacklist
       "\\|"))))

(defun my-tabs--parse-line (line)
  "Parse a tab-separated LINE into a plist with :title and :url."
  (let ((parts (split-string line "\t" t)))
    (list :id (nth 0 parts) :title (nth 1 parts) :url (nth 2 parts))))

(defun my-tabs--brotab-command (command)
  (async-shell-command (format "brotab %s" command)))

(provide 'my-tabs)

;;; my-tabs.el ends here
