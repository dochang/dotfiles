(defun $go-mode-hook ()
  ($camel-case-mode 1))

(setup (:package go-mode)

  (:hook $go-mode-hook)

  )
