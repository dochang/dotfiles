(req-package emmet-mode
  :bind (:map emmet-mode-keymap
         ("C-j" . nil))
  :init
  (setq emmet-move-cursor-between-quotes t))
