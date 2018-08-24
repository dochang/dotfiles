;;; Message Mode
;;;
;;; ** What is Message Mode?
;;;
;;;    Message mode is an alternative to mail mode for composing and
;;;    sending messages inside emacs.  It's the preferred mode used by
;;;    gnus.  However it can be used independently from gnus.

(define-advice message-unique-id (:filter-return (unique-id) by-uuid)
  "Return an UUID if available.  Otherwise, return the original
return value of `message-unique-id'."
  (let ((uuid ($uuid)))
    (if ($uuidgen-p uuid)
        uuid
      unique-id)))

(req-package message
  :ensure nil

  :init

  ;; Set domain part of Message-Ids to a fully qualified domain name.
  ;; [[info:message#News%20Headers]]
  (setq message-user-fqdn (or (bound-and-true-p message-user-fqdn)
                              (let ((parts (split-string user-mail-address "@")))
                                (and (> (length parts) 1) (last parts)))
                              "mail.gmail.com"))

  ;; `message-from-style' overrides `mail-from-style' in message mode.
  ;; [[info:message#Message%20Headers]]
  ;; [[info:message#News%20Headers]]
  (setq message-from-style 'angles)

  ;; Turn off auto-fill-mode, but filling can be done by manual.
  (setq message-fill-column nil)

  ;; Do not use `unsent'.  `C-u C-x m' cannot switch to such a
  ;; buffer whose name begins with "*unsent ".
  (setq message-generate-new-buffers 'unique)

  ;; Ask for confirmation when sending a message.
  (setq message-confirm-send t)

  ;; Kill the message buffer after sending a message.
  (setq message-kill-buffer-on-exit t)

  ;; Use the From: header for the envelope-from when sending mail with
  ;; sendmail.  Do not use `user-mail-address'.
  (setq message-sendmail-envelope-from 'header))
