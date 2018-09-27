(defun $mu4e-compose-mode-hook ()
  (save-excursion
    (let ((bcc-addr (message-field-value "From")))
      (message-add-header (format "Bcc: %s" bcc-addr)))))

(defun $mu4e-action-view-in-eww (msg)
  (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))

(req-package mu4e
  :ensure nil

  :hook (mu4e-compose-mode . $mu4e-compose-mode-hook)

  :custom
  (mu4e-maildir (expand-file-name (or (getenv "MAILDIR")) "~/Maildir"))
  (mu4e-attachment-dir (expand-file-name "~/Downloads/"))
  (mu4e-change-filenames-when-moving t)
  ;; Change the filename when moving messages to different folders.
  ;;
  ;; http://isync.sourceforge.net/mbsync.html#RECOMMENDATIONS
  (mu4e-update-interval nil)
  ;; Don't update mu automatically.
  (mu4e-view-show-addresses t)
  (mu4e-headers-date-format "%F")
  (mu4e-headers-time-format "%T%z")
  (mu4e-headers-long-date-format "%FT%T%z")
  (mu4e-compose-format-flowed nil)
  (mu4e-context-policy 'ask)
  (mu4e-compose-context-policy 'ask)

  :config
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "dochang@gmail.com"
          :enter-func (lambda () (mu4e-message "Entering context dochang@gmail.com"))
          :leave-func (lambda () (mu4e-message "Leaving context dochang@gmail.com"))
          :match-func (lambda (msg)
                        (or (null msg)
                            (mu4e-message-contact-field-matches
                             msg '(:from :to :cc :bcc) "dochang@gmail.com")))
          :vars '((user-mail-address . "dochang@gmail.com")
                  (user-full-name . "Desmond O. Chang")
                  (mu4e-compose-signature . nil)
                  (mu4e-compose-format-flowed . nil)))))
  (setq mu4e-user-mail-address-list
        (delq nil
              (mapcar (lambda (context)
                        (cdr (assq 'user-mail-address
                                   (mu4e-context-vars context))))
                      mu4e-contexts)))
  ;; This sets `mu4e-user-mail-address-list' to the concatenation of all
  ;; `user-mail-address' values for all contexts. If you have other mail
  ;; addresses as well, you'll need to add those manually.
  ;;
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Some-context-tricks.html#Some-context-tricks
  (setq mu4e-view-actions
        (append mu4e-view-actions
                '(("view in browser" . mu4e-action-view-in-browser)
                  ("view in eww" . $mu4e-action-view-in-eww)))))
