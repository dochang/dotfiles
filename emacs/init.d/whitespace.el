(req-package whitespace
  :hook ((prog-mode . whitespace-mode)
         ((conf-mode ledger-mode) . whitespace-mode))
  :init
  ;; `tab-mark` hurts my eyes.  Do not include it.
  (setq whitespace-style '(face trailing tabs lines-tail newline)))
