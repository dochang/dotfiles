(setup emms-playlist-mode
  (:package emms)

  (:with-feature emms-setup
    (:when-loaded
      (:require emms-playlist-mode)))

  )
