(setup (:package emms)

  (:with-feature dired
    (:bind "E" emms-play-dired))

  (:when-loaded

    (emms-minimalistic)

    (:require emms-playlist-mode)

    (:option emms-player-list '(emms-player-mpv))

    )

  )
