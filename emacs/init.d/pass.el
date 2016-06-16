;;; pass
(defun $pass-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil)
  (view-mode -1))

(req-package pass
  :init
  (add-hook 'pass-mode-hook '$pass-mode-hook))
