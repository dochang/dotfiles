;;; Dired Mode

;; Adding functions to `dired-load-hook' has no effect.  `dired' is an essential
;; package which may be loaded during the startup.

(setup dired

  (:when-loaded

    (:with-mode (dired-mode)
      (:hook dired-hide-details-mode))

    (setopt dired-listing-switches "-lhA")

    (setopt dired-dwim-target t)

    (setopt dired-guess-shell-alist-user
            (mapcar (lambda (elem) (list elem "xdg-open"))
                    '("\\.\\(mpe?g\\|avi\\|mkv\\)$"
                      "\\.\\(flv\\|rmvb\\|wmv\\|mp4\\|3gp\\)$"
                      "\\.\\(ogg\\|mp3\\)$"
                      "\\.\\(xbm\\|p[bgpn]m\\)$"
                      "\\.\\(jpe?g\\|gif\\|tif\\|png\\)$")))

    )

  )
