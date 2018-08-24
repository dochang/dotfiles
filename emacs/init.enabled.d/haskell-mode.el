;;; Haskell Mode

(defun $haskell-mode-hook ()
  ($run-prog-mode-hook))

(req-package haskell-mode
  :hook (haskell-mode . $haskell-mode-hook))
