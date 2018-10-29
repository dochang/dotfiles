(req-package yaml-path
  :ensure (yaml-path :pin :el-get-bundle)
  :el-get-bundle (yaml-path
                  :website "https://github.com/craig-ludington/yaml-path"
                  :description "Extends yaml-mode to display the path of the current yaml element in the message area."
                  :type github
                  :pkgname "craig-ludington/yaml-path")
  :bind (:map yaml-mode-map
         ("C-c C-p" . yaml-path/path)))
