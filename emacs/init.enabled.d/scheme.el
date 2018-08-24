;;; Scheme Mode

(defun $scheme-mode-hook ()
  ($lisp-mode-common-hook))

(req-package scheme
  :hook (scheme-mode . $scheme-mode-hook))
