(setup (:package emms)

  (:when-loaded

    (emms-minimalistic)

    (require 'emms-playlist-mode)

    (setopt emms-player-list '(emms-player-mpv))

    )

  )
