;;; Checking and Correcting Spelling

(req-package ispell
  :init
  ;; Use aspell for spell-checking.
  (setq ispell-program-name "aspell"))
