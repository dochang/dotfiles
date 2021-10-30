(req-package neotree
  :bind (("C-c t" . neotree-toggle))
  :init
  (setq neo-show-updir-line t)
  (setq neo-show-slash-for-folder t)
  (setq neo-reset-size-on-open nil)
  (setq neo-theme 'icons)
  (setq neo-smart-open nil)
  (setq neo-show-hidden-files nil)
  (setq neo-autorefresh t)
  (setq neo-window-fixed-size t)
  (setq neo-toggle-window-keep-p nil)
  (setq neo-hide-cursor t))
