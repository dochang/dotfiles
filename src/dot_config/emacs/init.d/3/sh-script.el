(defun $sh-base-mode-hook ()
  (smartparens-strict-mode -1))

(setup (:package sh-script)

  (add-hook 'sh-base-mode-hook '$sh-base-mode-hook)

  )
