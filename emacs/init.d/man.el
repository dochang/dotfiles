;;; Man

(defun $Man-mode-hook ()
  (scroll-lock-mode 1))

(req-package man
  :init
  ;; Make the manpage the current buffer in the current window
  (setq Man-notify-method 'pushy)
  (add-hook 'Man-mode-hook '$Man-mode-hook))
