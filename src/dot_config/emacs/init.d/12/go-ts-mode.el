(defun $go-ts-mode-hook ()
  ($camel-case-mode 1))

(setup (:package go-ts-mode)

  (:hook $go-ts-mode-hook)

  )
