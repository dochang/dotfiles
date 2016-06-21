;;; Scheme Mode

(defun $scheme-mode-hook ()
  ($prog-mode-hook*)
  ($lisp-mode-common-hook))

(req-package scheme
  :loader :built-in
  :init
  (add-hook 'scheme-mode-hook '$scheme-mode-hook))
