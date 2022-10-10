;;; Checking and Correcting Spelling

(req-package ispell
  :init
  ;; Use Enchant 2 for spell-checking.
  (setq ispell-program-name "enchant-2"))
