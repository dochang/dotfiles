;;; Dired Mode

;; Adding functions to `dired-load-hook' has no effect.  `dired' is an essential
;; package which may be loaded during the startup.

(setup dired

  (:also-load dired-x)

  (:when-loaded

    (:with-mode (dired-mode)
      (:hook dired-hide-details-mode))

    (:option dired-listing-switches "-lhA")

    (:option dired-dwim-target t)

    )

  )
