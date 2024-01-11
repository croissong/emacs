;;; my-snippets.el --- functions from somewhere else -*- lexical-binding: t; -*-

;; https://gist.github.com/hyOzd/23b87e96d43bca0f0b52
;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun my-snippets-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))



(defun my-snippets-kill-buffer-path (absolute-p)
  "Copy the current buffer's project-root-relative.
If not within a project, or with prefix argument, copy the absolute path instead."
  (interactive "P")
  (if-let ((relative (not absolute-p))
           (project (project-current)))
      (kill-new (file-relative-name buffer-file-name (project-root project)))
    (kill-new (if (equal major-mode 'dired-mode)
                  default-directory
                buffer-file-name))))


(provide 'my-snippets)

;;; my-snippets.el ends here
