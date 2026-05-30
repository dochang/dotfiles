(defun $go-mode-hook ()
  ($camel-case-mode 1))

(setup (:package go-mode)

  (:hook $go-mode-hook)

  )

(defun $go-ts-mode-hook ()
  ($camel-case-mode 1))

(setup (:package go-ts-mode)

  (:hook $go-ts-mode-hook)

  )
