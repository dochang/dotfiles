(req-package adoc-mode
  :require asciidoc
  :mode (("\\.adoc\\'" . adoc-mode)
         ("\\.asciidoc\\'" . adoc-mode))
  :config
  ($load-asciidoc adoc-mode-map))
