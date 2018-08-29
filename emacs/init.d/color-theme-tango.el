(req-package color-theme-tango
  :ensure nil
  :el-get t
  ;; Unable to use :el-get-bundle here.  color-theme-tango depends on
  ;; color-theme in the original el-get recipes.  el-get-bundle merge the
  ;; original dependencies even if you explicitly declare no dependencies.
  ;;
  ;; So we have to put the modified recipe in `el-get-sources'.
  :commands (color-theme-tango)
  :init
  (add-to-list '**color-themes** 'color-theme-tango))
