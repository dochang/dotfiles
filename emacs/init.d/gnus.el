(req-package gnus
  :bind (([XF86Mail] . gnus))
  :custom
  ;; Inhibit the startup message.
  ;;
  ;; This variable should be set in `.emacs' instead of `.gnus'.
  ;;
  ;; [[info:gnus#Startup%20Variables]]
  (gnus-inhibit-startup-message t)

  ;; Documentation is wrong.  Default will try "~/.authinfo" first.
  (auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

  ;; `shr' fails to display some HTML mails.  Use `gnus-w3m' instead.
  (mm-text-html-renderer 'gnus-w3m))
