(req-package darkclean-theme
  :ensure nil
  :quelpa (darkclean-theme
           :fetcher github
           :repo "Ferk/emacs.d"
           :files ("themes/darkclean-theme.el"))
  :init
  (let ((load-file-name (locate-library "darkclean-theme")))
    (when (and (boundp 'custom-theme-load-path) load-file-name)
      (add-to-list 'custom-theme-load-path
                   (file-name-as-directory (file-name-directory load-file-name)))))
  (add-to-list '**custom-themes** 'darkclean))
