(req-package dired-x
  :ensure nil
  :after dired
  :commands (dired-omit-mode)
  :init
  (setq dired-guess-shell-alist-user
        (mapcar (lambda (elem) (list elem "xdg-open"))
                '("\\.\\(mpe?g\\|avi\\|mkv\\)$"
                  "\\.\\(flv\\|rmvb\\|wmv\\|mp4\\|3gp\\)$"
                  "\\.\\(ogg\\|mp3\\)$"
                  "\\.\\(xbm\\|p[bgpn]m\\)$"
                  "\\.\\(jpe?g\\|gif\\|tif\\|png\\)$")))
  (require 'dired-x))
