(req-package whitespace
  :loader :built-in
  :init
  ;; `tab-mark` hurts my eyes.  Do not include it.
  (setq whitespace-style '(face trailing tabs lines-tail newline)))
