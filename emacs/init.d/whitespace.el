(req-package whitespace
  :hook ((prog-mode . whitespace-mode)
         ((conf-mode ledger-mode) . whitespace-mode))
  :custom
  ;; `tab-mark` hurts my eyes.  Do not include it.
  (whitespace-style '(face trailing tabs lines-tail newline)))
