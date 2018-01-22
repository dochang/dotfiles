(req-package zenburn
  :el-get t
  :init
  (autoload 'color-theme-zenburn "color-theme-zenburn" nil t)
  (add-to-list '**color-themes** 'color-theme-zenburn))
