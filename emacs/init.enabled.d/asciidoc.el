(defun $load-asciidoc (global-map)
  (easy-menu-define
    asciidoc-global-menu global-map "" asciidoc-global-menuspec))

(req-package asciidoc
  :ensure nil
  :el-get t)
