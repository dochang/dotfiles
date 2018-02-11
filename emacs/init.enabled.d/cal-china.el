(if (version< emacs-version "25.1")
    (req-package cal-china-plus
      :el-get t)
  ;; cal-china-plus has been merged into Emacs 25.1
  (req-package cal-china))
