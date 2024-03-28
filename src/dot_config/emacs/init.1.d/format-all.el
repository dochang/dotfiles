(setup (:package format-all)

  ;; (:with-mode (prog-mode conf-mode text-mode)
  ;;   (:hook format-all-mode))

  (:with-mode format-all-mode
    (:hook format-all-ensure-formatter))

  (defun $format-all-add-local-default-formatter (lang-id formatters)
    (unless (listp formatters)
      (setq formatters (list formatters)))
    (if (assoc lang-id format-all-formatters)
        format-all-formatters
      (cons (cons lang-id formatters)
            format-all-formatters)))

  (defun $format-all-set-local-default-formatter (lang-id formatters)
    (setq format-all-formatters
          ($format-all-add-local-default-formatter lang-id formatters)))

  (define-advice format-all-ensure-formatter (:before () set-default-formatter)
    (cond
     ((derived-mode-p 'python-mode)
      ($format-all-set-local-default-formatter "Python" '(black isort)))
     ((derived-mode-p 'lua-mode)
      ($format-all-set-local-default-formatter "Lua" 'stylua))
     ((derived-mode-p 'toml-mode)
      ($format-all-set-local-default-formatter "TOML" 'taplo-fmt))
     ))

  )
