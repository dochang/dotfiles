;;; * Mail Config
;;;
;;; ** How to compute the mail address?
;;;
;;;    - [[info:emacs#Mail%20Headers]]
;;;    - [[info:message#News%20Headers]]
;;;
;;; *** User Full Name
;;;
;;;     1. =(user-full-name)=
;;;
;;; *** Domain Part
;;;
;;;     1. =message-user-fqdn=
;;;     2. =(system-name)=
;;;     3. =mail-host-address=
;;;     4. =message-user-mail-address= (i.e. =user-mail-address=)
;;;
;;; *** Mail Address
;;;
;;;     1. =user-mail-address=
;;;
;;;        1. $EMAIL
;;;        2. =(user-login-name)= @ =mail-host-address=
;;;        3. =(user-login-name)= @ =(system-name)=
;;;
;;;           1. =(system-name)= returns =system-name=
;;;
;;; ** API
;;;
;;; *** [[info:elisp#User%20Identification]]
;;;
;;;     - =(user-full-name)=
;;;
;;;       Returns =user-full-name=, which is initialized by =$NAME=
;;;       or a system-defined value.
;;;
;;;     - =user-mail-address=
;;;
;;;     - =(user-login-name)=
;;;
;;;       Returns =user-login-name=.
;;;
;;; *** [[info:elisp#System%20Environment]]
;;;
;;;     - =mail-host-address=
;;;
;;;     - =(system-name)=
;;;
;;;       Returns =system-name=.
;;;
;;; ** NOTES
;;;
;;; 1. Setting `message-send-mail-function' is unnecessary.  Emacs can
;;;    choose a suitable default value for it.
;;;
;;;    [[info:message#Mail%20Variables]]
;;;

(req-package sendmail

  :init

  (setq mail-from-style 'angles)

  ;; Insert BCC to self in messages to be sent.
  (setq mail-self-blind t)

  ;; Use the From: header for the envelope-from when sending mail.  Do not
  ;; use `user-mail-address'.
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)

  ;; Send mail using `sendmail'.
  ;; [[info:emacs#Mail%20Sending]]
  (setq send-mail-function 'sendmail-send-it)

  )
