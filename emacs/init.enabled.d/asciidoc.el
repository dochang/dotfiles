(defun $load-asciidoc (global-map)
  (easy-menu-define
    asciidoc-global-menu global-map "" asciidoc-global-menuspec))

(req-package asciidoc
  :ensure nil
  :quelpa (asciidoc :fetcher github :repo "metaperl/asciidoc-el"))
