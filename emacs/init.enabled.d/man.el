;;; Man

(defun $Man-mode-hook ()
  (scroll-lock-mode 1))

(req-package man
  :hook (Man-mode . $Man-mode-hook)
  :init
  ;; Make the manpage the current buffer in the current window
  (setq Man-notify-method 'pushy))
