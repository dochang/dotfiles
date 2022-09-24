(eval-and-compile
  (require 'req-package))

(req-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)
         ("\\.asciidoc\\'" . adoc-mode)))
