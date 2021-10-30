(req-package emms-player-mplayer
  :ensure emms
  :after (emms emms-player-simple)
  :config
  ($emms-player-set emms-player-mplayer))
