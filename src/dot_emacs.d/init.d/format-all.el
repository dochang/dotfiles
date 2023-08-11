(req-package format-all

  :hook (((prog-mode conf-mode) . format-all-mode)
         ((prog-mode conf-mode) . format-all-ensure-formatter))

  :config

  ;; Support isort for Python.
  (define-format-all-formatter isort
    (:executable "isort")
    (:install "pip install isort[requirements,pipfile,pyproject]")
    (:languages "Python")
    (:features)
    (:format (format-all--buffer-easy
              executable "-q" "-")))

  (setq format-all-default-formatters
        (append '(("Lua" stylua)
                  ("TOML" taplo-fmt))
                format-all-default-formatters))

  )
