;;; my-snippets.el --- functions from somewhere else -*- lexical-binding: t; -*-

;; https://gist.github.com/hyOzd/23b87e96d43bca0f0b52
;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun my-snippets-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p
             (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))


(defun my-snippets-copy-buffer-path (absolute-p)
  "Copy the current buffer's project-root-relative.
If not within a project, or with prefix argument, copy the absolute path instead."
  (interactive "P")
  (if-let ((buffer-path (my-snippets--get-buffer-path)))
    (if (not absolute-p)
        (if-let ((project (project-current))
                 (root (project-root project)))
          (kill-new (file-relative-name buffer-path root))
          (message "Not in project"))
      (kill-new buffer-path))))

(defun my-snippets-copy-buffer-dir (absolute-p)
  "Copy the directory path of the current buffer.
If not within a project, or with prefix argument ABSOLUTE-P, copy the absolute path instead."
  (interactive "P")
  (if-let ((buffer-path (my-snippets--get-buffer-path)))
    (let ((dir-path (file-name-directory buffer-path)))
      (if (and (not absolute-p) (project-current))
          (if-let ((project (project-current))
                   (root (project-root project)))
            (progn
              (kill-new (file-relative-name dir-path root))
              (message "Copied relative directory path: %s"
                       (file-relative-name dir-path root)))
            (message "Not in project"))
        (progn
          (kill-new dir-path)
          (message "Copied absolute directory path: %s" dir-path))))
    (message "Unable to determine buffer path")))

(defun my-snippets-copy-buffer-file-name ()
  "Copy the file name of the current buffer."
  (interactive "")
  (kill-new (file-name-nondirectory (my-snippets--get-buffer-path))))

(defun my-snippets--get-buffer-path ()
  (let ((buffer (current-buffer)))
    (cond
     (buffer-file-name
      buffer-file-name)

     ;; If it's a dired buffer, return the directory
     ((derived-mode-p 'dired-mode)
      (dired-current-directory))

     ;; If it's in a project, return the relative path to the project root
     ((and (fboundp 'project-current) (project-current))
      (project-root (project-current)))
     ;; Otherwise, return the buffer file name or buffer name
     )))


(provide 'my-snippets)

;;; my-snippets.el ends here
