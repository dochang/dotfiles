(setup browse-url

  (defun $browse-url-clipboard (url &rest args)
    (interactive (browse-url-interactive-arg "URL: "))
    (with-temp-buffer
      (insert url)
      (copy-region-as-kill (point-min) (point-max))))

  (defun $browse-url-browser-select (url &rest args)
    (interactive (browse-url-interactive-arg "URL: "))
    (let* ((browser-alist '(("copy" . $browse-url-clipboard)
                            ("eww" . eww-browse-url)
                            ("xdg" . browse-url-xdg-open)))
           (browser-key (completing-read "Browser: " browser-alist
                                         nil t nil nil))
           (browser (if (or (null browser-key)
                            (string-blank-p browser-key))
                        (cdar browser-alist)
                      (cdr (assoc browser-key browser-alist)))))
      (apply browser url args)))

  (:when-loaded

    (:option browse-url-browser-function '$browse-url-browser-select))

  )
