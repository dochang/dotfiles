(req-package gruvbox-theme
  :custom
  (**custom-themes**
   (cl-remove-duplicates
    (append **custom-themes**
            '((gruvbox)
              (gruvbox-dark-medium)
              (gruvbox-dark-soft)
              (gruvbox-dark-hard))
            '())
    :key 'car)))
