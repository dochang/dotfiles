(req-package emmet-mode
  :bind (:map emmet-mode-keymap
         ("C-j" . nil))
  :hook (css-mode
         handlebars-mode
         mustache-mode
         sgml-mode
         web-mode)
  :init
  (setq emmet-move-cursor-between-quotes t))
