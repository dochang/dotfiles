(req-package dream-theme
  :ensure nil
  ;; We can't install it via quelpa due to this error:
  ;;
  ;; ```
  ;; Package lacks a file header
  ;; ```
  ;;
  ;; Use el-get instead.
  :el-get-bundle (dream-theme
                  :website "https://emacsthemes.com/themes/dream-theme.html"
                  :description "Dark, clean theme for emacs. inspired by the zenburn, sinburn."
                  :depends ()
                   ;; Do not depend on color-theme, or el-get will install
                   ;; color-theme.  Instead, install color-theme by package.el.
                  :type github
                  :pkgname "djcb/dream-theme")
  :init
  (let ((load-file-name (locate-library "dream-theme")))
    (when (and (boundp 'custom-theme-load-path) load-file-name)
      (add-to-list 'custom-theme-load-path
                   (file-name-as-directory (file-name-directory load-file-name)))))
  (add-to-list '**custom-themes** 'dream))
