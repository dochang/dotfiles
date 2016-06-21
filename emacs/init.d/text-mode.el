;;; Text Mode

(defun $text-mode-hook ()
  (flyspell-mode 1))

(req-package text-mode
  :init
  (add-hook 'text-mode-hook '$text-mode-hook))
