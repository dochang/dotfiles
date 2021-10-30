;; https://www.emacswiki.org/emacs/ColorThemeCollection
;; http://edward.oconnor.cx/config/elisp/hober2-theme.el
;; http://tess.oconnor.cx/config/
;; https://github.com/articuluxe/hober2-theme

(req-package hober2-theme
  :ensure (hober2-theme :pin :quelpa)
  :quelpa (hober2-theme
           :fetcher github
           :repo "articuluxe/hober2-theme")
  :init
  (let ((load-file-name (locate-library "hober2-theme")))
    (when (and (boundp 'custom-theme-load-path) load-file-name)
      (add-to-list 'custom-theme-load-path
                   (file-name-as-directory (file-name-directory load-file-name)))))
  (setq **custom-themes** ($add-theme **custom-themes** 'hober2)))
