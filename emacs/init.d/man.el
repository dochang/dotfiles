;;; Man

(req-package man
  :custom
  ;; Make the manpage the current buffer in the current window
  (Man-notify-method 'pushy))
