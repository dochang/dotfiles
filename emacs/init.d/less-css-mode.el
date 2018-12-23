;;; LESS CSS Mode
;; [[http://www.emacswiki.org/emacs/LessCssMode]]
;; [[https://github.com/purcell/less-css-mode]]

;; This package has been included in Emacs core at commit
;; 3f887812e708123eca2f85cfbf5004e405aff914.
(req-package less-css-mode
  :ensure nil
  :custom
  ;; Don't compile after saving.
  (less-css-compile-at-save nil)
  :init
  (when (version< emacs-version "26")
    (package-install 'less-css-mode)))
