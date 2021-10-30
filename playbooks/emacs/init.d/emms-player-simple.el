(defun $emms-player-set (player)
  (emms-player-set
   player
   'regex
   (concat "\\`\\(https?\\|mms\\)://\\|"
           (apply #'emms-player-simple-regexp
                  emms-player-base-format-list))))

(req-package emms-player-simple
  :ensure emms)
