(req-package lua2-mode
  :ensure nil
  :el-get-bundle (lua2-mode
                  :website "http://www.enyo.de/fw/software/lua-emacs/lua2-mode.html"
                  :description "A semantic highlighting extension for lua-mode"
                  :depends ()
                   ;; Do not depend on lua-mode, or el-get will install
                   ;; lua-mode.  Instead, install lua-mode by package.el.
                  :type http
                  :url "http://www.enyo.de/fw/software/lua-emacs/lua2-mode.el"))
