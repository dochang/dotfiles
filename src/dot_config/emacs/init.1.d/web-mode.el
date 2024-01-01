(defun $web-mode-hook ()
  (when (and web-mode-enable-engine-detection
             (local-variable-p 'engine))
    (web-mode-set-engine (buffer-local-value 'engine))))

(setup (:package web-mode)

  (:hook $web-mode-hook)

  (setq auto-mode-alist
        (append '(("\\.\\(tmpl\\|mustache\\|hbs\\|ejs\\|eta\\|sqrl\\)\\'" . web-mode))
                auto-mode-alist))

  (:when-loaded

    (:option web-mode-enable-engine-detection t)

    (setq web-mode-engine-file-regexps
          (append '(("ctemplate" . "\\.sqrl\\'")
                    ("ejs" . "\\.eta\\'"))
                  web-mode-engine-file-regexps))

    )

  )
