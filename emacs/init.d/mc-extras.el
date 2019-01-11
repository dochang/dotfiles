(req-package mc-extras

  :init

  (eval-after-load 'multiple-cursors
    '(progn
       (define-key mc/keymap (kbd "C-. M-C-f") 'mc/mark-next-sexps)
       (define-key mc/keymap (kbd "C-. M-C-b") 'mc/mark-previous-sexps)
       (define-key mc/keymap (kbd "C-. <") 'mc/mark-all-above)
       (define-key mc/keymap (kbd "C-. >") 'mc/mark-all-below)

       (define-key mc/keymap (kbd "C-. C-d") 'mc/remove-current-cursor)
       (define-key mc/keymap (kbd "C-. C-k") 'mc/remove-cursors-at-eol)
       (define-key mc/keymap (kbd "C-. d")   'mc/remove-duplicated-cursors)
       (define-key mc/keymap (kbd "C-. C-o") 'mc/remove-cursors-on-blank-lines)

       (define-key mc/keymap (kbd "C-. C-.") 'mc/freeze-fake-cursors-dwim)

       (define-key mc/keymap (kbd "C-. .")   'mc/move-to-column)
       (define-key mc/keymap (kbd "C-. =")   'mc/compare-chars)))

  (eval-after-load 'rect
    ;; Emacs 24.4+ comes with rectangle-mark-mode.
    '(define-key rectangle-mark-mode-map (kbd "C-. C-,")
       'mc/rect-rectangle-to-multiple-cursors))

  (eval-after-load 'cua-rect
    '(define-key cua--rectangle-keymap   (kbd "C-. C-,")
       'mc/cua-rectangle-to-multiple-cursors))

  )
