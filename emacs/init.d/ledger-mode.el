;;; Ledger Mode
;; [[http://www.ledger-cli.org/]]
;; [[https://github.com/ledger/ledger]]

(defun $ledger-mode-hook ()
  (whitespace-mode 1))

(defun $ledger-reconcile-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil))

(req-package ledger-mode
  :init
  ;; Do not highlight transaction under point.
  (setq ledger-highlight-xact-under-point nil)
  (add-hook 'ledger-mode-hook '$ledger-mode-hook)
  (add-hook 'ledger-reconcile-mode-hook '$ledger-reconcile-mode-hook))
