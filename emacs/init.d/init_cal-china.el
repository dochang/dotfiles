(if (version< emacs-version "25.1")
    (req-package cal-china-plus
      :loader :el-get)
  ;; cal-china-plus has been merged into Emacs 25.1
  (req-package cal-china
    :loader :path))
