(req-package emms-player-mpv
  :ensure emms
  :after (emms emms-player-simple)
  :config
  ($emms-player-set emms-player-mpv))
