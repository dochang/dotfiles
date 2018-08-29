(req-package linum
  :hook ((prog-mode . linum-mode)
         ((adoc-mode conf-mode org-mode) . linum-mode)))
