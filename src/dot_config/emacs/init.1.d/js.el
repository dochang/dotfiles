(defun $js-base-mode-hook ()
  ($camel-case-mode 1))

(setup js

  (setq auto-mode-alist
        (append '(("\\.[cm]js\\'" . js-mode))
                auto-mode-alist))

  (add-hook 'js-base-mode-hook '$js-base-mode-hook)

  )
