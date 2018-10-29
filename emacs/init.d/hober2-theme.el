;; https://www.emacswiki.org/emacs/ColorThemeCollection
;; http://edward.oconnor.cx/config/elisp/hober2-theme.el
;; http://tess.oconnor.cx/config/

(req-package hober2-theme
  :ensure (hober2-theme :pin :quelpa)
  :quelpa (hober2-theme
           :fetcher url
           :url "http://edward.oconnor.cx/config/elisp/hober2-theme.el")
  :init
  (let ((load-file-name (locate-library "hober2-theme")))
    (when (and (boundp 'custom-theme-load-path) load-file-name)
      (add-to-list 'custom-theme-load-path
                   (file-name-as-directory (file-name-directory load-file-name)))))
  (add-to-list '**custom-themes** 'hober2))
