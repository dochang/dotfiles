;; There are 2 versions.
;;
;;  1. https://github.com/ruby/elisp/blob/master/rdoc-mode.el
;;  2. [[https://github.com/jwiegley/ruby-mode/blob/master/rdoc-mode.el]]
;;
;; 1 is newer than 2.

(req-package rdoc-mode
  :ensure (rdoc-mode :pin :el-get-bundle)
  :el-get-bundle (rdoc-mode
                  :website "https://github.com/ruby/elisp/blob/master/rdoc-mode.el"
                  :description "Major mode for RDoc editing"
                  :type http
                  :url "https://github.com/ruby/elisp/raw/master/rdoc-mode.el")
  :mode ("\\.rdoc\\'" . rdoc-mode))
