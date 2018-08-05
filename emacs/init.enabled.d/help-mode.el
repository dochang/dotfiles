;;; Help Mode

(defun $help-mode-hook ()
  (scroll-lock-mode 1))

(req-package help-mode
  :ensure nil
  :init
  (add-hook 'help-mode-hook '$help-mode-hook))
