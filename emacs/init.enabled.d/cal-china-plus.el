;; cal-china-plus has been merged into Emacs 25.1
(when (version< emacs-version "25.1")
  (req-package cal-china-plus
    :el-get t))
