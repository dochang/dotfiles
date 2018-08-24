;;; Haskell Mode

(defun $haskell-mode-hook ()
  ($run-prog-mode-hook))

(req-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook '$haskell-mode-hook))
