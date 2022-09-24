(req-package hippie-exp
  :bind (("M-/" . hippie-expand))
  :config
  ;; Don't try to complete an entire line or list.  Those functions don't work
  ;; well with paredit
  ;;
  ;; https://emacs.stackexchange.com/a/4249
  ;; https://www.emacswiki.org/emacs/HippieExpand#toc9
  (setq hippie-expand-try-functions-list
        (cl-reduce (lambda (fn-list fn)
                     (remove fn fn-list))
                   '(try-expand-line try-expand-list)
                   :initial-value hippie-expand-try-functions-list)))
