;;; CSS Mode

(defun $css-mode-hook ()
  ($prog-mode-hook*)
  (emmet-mode 1)
  (rainbow-mode 1))

(req-package css-mode
  :init
  (add-hook 'css-mode-hook '$css-mode-hook))
