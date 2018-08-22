(req-package cflow
  :ensure nil
  :el-get t
  ;; The repo size of cflow is small, but the git repo has a module, gnulib,
  ;; which is a big repo.  We can't `git clone` cflow via quelpa, because it
  ;; always downloads git modules.  We should install cflow via quelpa's "url"
  ;; fetcher, but we can't, due to this error:
  ;;
  ;; ```
  ;; Error getting PACKAGE-DESC: (search-failed ;;; cflow.el ends here)
  ;; ```
  ;;
  ;; Install cflow via el-get instead.
  :mode ("\\.cflow$" . cflow-mode))
