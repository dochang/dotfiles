(setup (:package emms)

  (:with-feature dired
    (:bind "E" emms-play-dired))

  (:when-loaded

    (:option emms-player-list '(emms-player-mpv))

    )

  )
