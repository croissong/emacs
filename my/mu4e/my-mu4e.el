;;; my-mu4e.el --- Mu4e config & customizations -*- lexical-binding: t; -*-

(defvar my-mu4e-account-alist
      '(("iogroup"
         (mu4e-sent-folder "/iogroup/sent")
         (user-mail-address "jm@datawerk.de")
         (smtpmail-smtp-user "jm@iogroup.org")
         (smtpmail-local-domain "iogroup.org")
         (smtpmail-default-smtp-server "smtp.office365.com")
         (smtpmail-smtp-server "smtp.office365.com")
         (smtpmail-stream-type starttls)
         (smtpmail-smtp-service 587)
         )
        ("gmail"
         (mu4e-drafts-folder "/[Google Mail].Drafts")
         (mu4e-sent-folder   "/[Google Mail].Sent Mail")
         (mu4e-trash-folder  "/[Google Mail].Trash")
         (user-mail-address "jan.moeller0@gmail.com")
         (smtpmail-smtp-user "jan.moeller0@gmail.com")
         (smtpmail-local-domain "gmail.com")
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-service 587)
         )))

(defun my-mu4e-set-account ()
      "Set the account for composing a message.
   This function is taken from:
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
      (let* ((account
              (if mu4e-compose-parent-message
                  (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                    (string-match "/\\(.*?\\)/" maildir)
                    (match-string 1 maildir))
                (completing-read (format "Compose with account: (%s) "
                                         (mapconcat #'(lambda (var) (car var))
                                                    my-mu4e-account-alist "/"))
                                 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                 nil t nil nil (caar my-mu4e-account-alist))))
             (account-vars (cdr (assoc account my-mu4e-account-alist))))
        (if account-vars
            (mapc #'(lambda (var)
                      (set (car var) (cadr var)))
                  account-vars)
          (error "No email account found"))))

(provide 'my-mu4e)
;;; my-util.el ends here
