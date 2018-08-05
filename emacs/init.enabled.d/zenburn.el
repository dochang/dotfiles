(req-package zenburn
  :ensure nil
  :el-get t
  :init
  (autoload 'color-theme-zenburn "color-theme-zenburn" nil t)
  (add-to-list '**color-themes** 'color-theme-zenburn))
