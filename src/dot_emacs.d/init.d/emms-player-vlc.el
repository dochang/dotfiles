(req-package emms-player-vlc
  :ensure emms
  :after (emms emms-player-simple)
  :config
  ($emms-player-set emms-player-vlc))
