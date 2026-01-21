(setup (:package clojure-mode))

(when (>= emacs-major-version 30)
  ;; https://github.com/clojure-emacs/clojure-ts-mode#why-does-clojure-ts-mode-require-emacs-30
  (setup (:package clojure-ts-mode)))
