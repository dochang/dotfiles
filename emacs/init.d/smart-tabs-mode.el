;; The feature of `smarttabs' is `smart-tabs-mode'.
;;
;; Install my version because of [[https://github.com/jcsalomon/smarttabs/pull/28]]

(req-package smart-tabs-mode
  :ensure (smart-tabs-mode :pin :quelpa)
  :quelpa (smart-tabs-mode
           :fetcher github
           :repo "dochang/smarttabs"
           :branch "keep-indent-setting"
           :files ("smart-tabs-mode.el")))
