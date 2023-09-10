(defun $prog-mode-hook ()
  (set (make-local-variable 'show-trailing-whitespace) t))

(setup prog-mode

  (:hook $prog-mode-hook)

  )
