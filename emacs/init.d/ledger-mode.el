;;; Ledger Mode
;; [[http://www.ledger-cli.org/]]
;; [[https://github.com/ledger/ledger]]

(defun $ledger-reconcile-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil))

(req-package ledger-mode
  :hook ((ledger-reconcile-mode . $ledger-reconcile-mode-hook))
  :custom
  ;; Do not highlight transaction under point.
  (ledger-highlight-xact-under-point nil))
