;;; EMMS
;; [[http://www.gnu.org/software/emms/]]

(req-package emms
  :after dired

  :bind (:map dired-mode-map
         ("E" . emms-play-dired))

  :init
  ;; Do not save playlist for EMMS.
  (setq emms-history-file nil)
  (setq emms-player-mpd-music-directory "~/media/music/")

  :config
  (emms-minimalistic)
  (require 'emms-playlist-mode)
  (make-directory emms-directory 'parents)
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-player-base-format-list
        (delete-dups
         (append emms-player-base-format-list
                 '("m4b" "m4p" "m4v" "m4r" "3gp" "3g2" "aac"))))
  (mapc (lambda (player)
          (emms-player-set
           player
           'regex
           (concat "\\`\\(https?\\|mms\\)://\\|"
                   (apply #'emms-player-simple-regexp
                          emms-player-base-format-list))))
        (list emms-player-mplayer
              emms-player-vlc
              emms-player-mpv)))
