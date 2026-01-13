(setup atomic-chrome

  (unless (package-installed-p 'atomic-chrome)

    (quelpa '(atomic-chrome :fetcher url
                            :url "https://github.com/KarimAziev/atomic-chrome/raw/refs/heads/master/atomic-chrome.el")
            :upgrade nil)
    ;; The git repo is too big to clone.  Download the file instead.

    )

  (add-hook 'emacs-startup-hook 'atomic-chrome-start-server)

  (:when-loaded

    (setopt atomic-chrome-extension-type-list
            '(atomic-chrome ghost-text))

    (setopt atomic-chrome-enable-auto-update t)

    (setopt atomic-chrome-enable-bidirectional-edit t)

    )

  )
