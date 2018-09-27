;;; LESS CSS Mode
;; [[http://www.emacswiki.org/emacs/LessCssMode]]
;; [[https://github.com/purcell/less-css-mode]]

(req-package less-css-mode
  :custom
  ;; Don't compile after saving.
  (less-css-compile-at-save nil))
