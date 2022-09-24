(req-package ansi-color
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  ;; Garbage ls/gcc output
  ;; [[http://www.emacswiki.org/emacs/AnsiColor]]
  )
