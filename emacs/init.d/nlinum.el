;; Why do I use nlinum instead of linum?
;;
;; https://github.com/rnkn/olivetti#known-bugs
;; https://github.com/rnkn/olivetti/pull/4#issuecomment-106675327
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20674#17
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20674#23

;; We only use `nlinum-mode' when Emacs < 26.  Use
;; `display-line-numbers-mode' for Emacs >= 26 instead.
(when (version< emacs-version "26")
  (req-package nlinum
    :hook ((prog-mode adoc-mode conf-mode org-mode) . nlinum-mode)
    :custom
    (nlinum-highlight-current-line t)
    (nlinum-format " %d ")))
