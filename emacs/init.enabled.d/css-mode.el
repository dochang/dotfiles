;;; CSS Mode

(defun $css-mode-hook ()
  (emmet-mode 1)
  (rainbow-mode 1))

(req-package css-mode
  :hook (css-mode . $css-mode-hook))
