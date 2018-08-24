;;; Help Mode

(defun $help-mode-hook ()
  (scroll-lock-mode 1))

(req-package help-mode
  :ensure nil
  :hook (help-mode . $help-mode-hook))
