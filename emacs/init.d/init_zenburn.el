(req-package zenburn
  :loader :el-get
  :init
  (autoload 'color-theme-zenburn "color-theme-zenburn" nil t)
  (add-to-list '**color-themes** 'color-theme-zenburn))
