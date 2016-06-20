;;; JSX Mode
;; [[https://github.com/jsx/jsx-mode.el]]

(defun $jsx-mode-hook ()
  ($prog-mode-hook*)
  (emmet-mode 1)
  ($camel-case-mode 1))

(req-package jsx-mode
  :mode ("\\.jsx\\'" . jsx-mode)
  :init
  (setq jsx-indent-level 4)
  (add-hook 'jsx-mode-hook '$jsx-mode-hook))
