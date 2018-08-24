(req-package whitespace
  :hook ((conf-mode) . whitespace-mode)
  :init
  ;; `tab-mark` hurts my eyes.  Do not include it.
  (setq whitespace-style '(face trailing tabs lines-tail newline)))
