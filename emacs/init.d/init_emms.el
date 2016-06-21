;;; EMMS
;; [[http://www.gnu.org/software/emms/]]

(req-package emms
  :init
  ;; Do not save playlist for EMMS.
  (setq emms-history-file nil)
  (setq emms-player-mpd-music-directory "~/media/music/")

  :config
  (emms-minimalistic)
  (require 'emms-playlist-mode)
  (emms-player-set emms-player-mplayer 'regex
                   (concat "\\`\\(http\\|mms\\)://\\|"
                           (emms-player-simple-regexp
                            "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
                            "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
                            "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
                            "flv")))
  (emms-default-players)
  (make-directory emms-directory)
  ;; mpv support for EMMS
  ;;
  ;; [[https://github.com/dochang/emms-player-mpv]]
  (require emms-player-mpv)
  (add-to-list 'emms-player-list 'emms-player-mpv))
