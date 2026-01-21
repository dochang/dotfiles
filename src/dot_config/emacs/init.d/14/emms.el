(setup (:package emms)

  (:when-loaded

    (emms-minimalistic)

    (require 'emms-playlist-mode)

    (setopt emms-player-list '(emms-player-mpv))

    )

  (with-eval-after-load 'emms-history

    (setopt emms-history-file nil)
    ;; Do not save playlist for EMMS.

    )

  )
