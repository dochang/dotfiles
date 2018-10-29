(req-package dired-x
  :ensure (dired-x :pin :built-in)
  :after dired
  :commands (dired-omit-mode)
  :custom
  (dired-guess-shell-alist-user
   (mapcar (lambda (elem) (list elem "xdg-open"))
           '("\\.\\(mpe?g\\|avi\\|mkv\\)$"
             "\\.\\(flv\\|rmvb\\|wmv\\|mp4\\|3gp\\)$"
             "\\.\\(ogg\\|mp3\\)$"
             "\\.\\(xbm\\|p[bgpn]m\\)$"
             "\\.\\(jpe?g\\|gif\\|tif\\|png\\)$")))
  :init
  (require 'dired-x))
