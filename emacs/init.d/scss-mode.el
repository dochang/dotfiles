;;; Scss Mode
;; [[https://github.com/antonj/scss-mode]]

(req-package scss-mode
  :custom
  ;; Don't compile after saving.
  (scss-compile-at-save nil))
