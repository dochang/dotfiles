(defun $typescript-ts-base-mode-hook ()
  (smartparens-strict-mode -1)
  ($camel-case-mode 1))

(setup (:package typescript-ts-mode)

  (setq auto-mode-alist
        (append '(("\\.ts\\'" . typescript-ts-mode)
                  ("\\.tsx\\'" . tsx-ts-mode))
                auto-mode-alist))

  (add-hook 'typescript-ts-base-mode-hook #'$typescript-ts-base-mode-hook)

  )
