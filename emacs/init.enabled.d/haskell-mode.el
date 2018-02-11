;;; Haskell Mode

(defun $haskell-mode-hook ()
  ($prog-mode-hook*))

(req-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook '$haskell-mode-hook))
