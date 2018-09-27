(req-package emmet-mode
  :bind (:map emmet-mode-keymap
         ("C-j" . nil))
  :hook (css-mode
         handlebars-mode
         mustache-mode
         sgml-mode
         web-mode)
  :custom
  (emmet-move-cursor-between-quotes t))
