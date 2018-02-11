(req-package zenburn-theme
  :init
  (add-to-list 'safe-local-eval-forms
               '(rainbow-mode 1))
  (add-to-list '**custom-themes** 'zenburn))
