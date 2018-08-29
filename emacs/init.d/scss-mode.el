;;; Scss Mode
;; [[https://github.com/antonj/scss-mode]]

(req-package scss-mode
  :init
  ;; Don't compile after saving.
  (setq scss-compile-at-save nil))
