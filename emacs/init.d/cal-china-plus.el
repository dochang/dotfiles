;; Chinese calendar support for anniversaries.
;;
;; - [[https://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00135.html]]
;; - [[http://permalink.gmane.org/gmane.emacs.sources/3252]]
;; - [[https://github.com/leoliu/cal-china-plus]]
;;
;; Has been merged into Emacs 25.1

(when (version< emacs-version "25.1")
  (req-package cal-china-plus
    :ensure (cal-china-plus :pin :quelpa)
    :quelpa (cal-china-plus :fetcher github :repo "leoliu/cal-china-plus")))
