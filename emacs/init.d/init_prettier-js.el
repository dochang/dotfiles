(define-advice prettier-js (:around (fn) if-prettier-exists)
  (ignore-errors (funcall fn)))

(req-package prettier-js)
