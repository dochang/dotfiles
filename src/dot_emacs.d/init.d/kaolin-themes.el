(req-package kaolin-themes
  :init
  (setq **custom-themes**
        (cl-remove-duplicates
         (append **custom-themes**
                 '((kaolin-aurora)
                   (kaolin-bubblegum . 'display-graphic-p)
                   ;; Can't see comments on kaolin-bubblegum
                   (kaolin-dark)
                   (kaolin-eclipse)
                   (kaolin-galaxy)
                   (kaolin-mono-dark)
                   (kaolin-ocean . 'display-graphic-p)
                   ;; Can't see comments on kaolin-ocean
                   (kaolin-temple . (lambda () nil))
                   ;; Can't work well with dimmer
                   (kaolin-valley-dark . (lambda () nil))
                   ;; Can't work well with dimmer
                   )
                 '())
         :key 'car)))
