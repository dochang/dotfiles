(define-advice prettier-js (:around (fn &rest r) if-prettier-exists)
  (ignore-errors (apply fn r)))

(req-package prettier-js)
