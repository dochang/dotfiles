(req-package kaolin-themes
  :custom
  (**custom-themes**
   (cl-remove-duplicates
    (append **custom-themes**
            '((kaolin-aurora)
              (kaolin-bubblegum)
              (kaolin-dark)
              (kaolin-eclipse)
              (kaolin-galaxy)
              (kaolin-mono-dark)
              (kaolin-ocean)
              (kaolin-temple)
              (kaolin-valley-dark))
            '())
    :key 'car)))
