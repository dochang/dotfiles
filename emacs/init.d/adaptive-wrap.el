;; We need this mode to solve the problem of displaying org-mode headlines and
;; source blocks in `visual-line-mode'.
;;
;; [1]: https://emacs.stackexchange.com/a/12437
;; [2]: https://orgmode.org/manual/Clean-view.html#FOOT144

(req-package adaptive-wrap
  :hook (org-mode . adaptive-wrap-prefix-mode))
