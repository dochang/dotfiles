(defun $adoc-mode-hook ()
  (linum-mode 1))

(req-package adoc-mode
  :require asciidoc
  :mode (("\\.adoc\\'" . adoc-mode)
         ("\\.asciidoc\\'" . adoc-mode))
  :hook (adoc-mode . $adoc-mode-hook)
  :config
  ($load-asciidoc adoc-mode-map))
