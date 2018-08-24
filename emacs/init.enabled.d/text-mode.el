;;; Text Mode

(defun $text-mode-hook ()
  (flyspell-mode 1))

(req-package text-mode
  :ensure nil
  :hook (text-mode . $text-mode-hook))
