;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;; https://ebzzry.com/en/emacs-pairs/
;; https://ebzzry.com/eo/emakso-paroj/
(setup (:package smartparens)

  (add-hook 'emacs-startup-hook 'smartparens-global-mode)
  (:with-mode (prog-mode)
    (:hook smartparens-strict-mode)
    (:hook show-smartparens-mode))

  (:also-load smartparens-config)

  )
