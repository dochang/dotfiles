(req-package use-package-el-get
  ;; force: Eval the form before `req-package-finish'.
  :force t
  ;; demand: Load the package when the form loaded.
  :demand t
  ;; ensure: Ensure the package installed when the form loaded.
  :ensure t
  :config
  (use-package-el-get-setup))
