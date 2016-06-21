(req-package gnus
  :bind (([XF86Mail] . gnus))
  :init
  ;; Inhibit the startup message.
  ;;
  ;; This variable should be set in `.emacs' instead of `.gnus'.
  ;;
  ;; [[info:gnus#Startup%20Variables]]
  (setq gnus-inhibit-startup-message t)

  ;; Documentation is wrong.  Default will try "~/.authinfo" first.
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

  ;; `shr' fails to display some HTML mails.  Use `gnus-w3m' instead.
  (setq mm-text-html-renderer 'gnus-w3m))
