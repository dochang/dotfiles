(req-package ivy
  :hook (emacs-startup . ivy-mode)
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-partial)
         ;; Make TAB only complete and do not open file or directory.
         ;;
         ;; https://github.com/abo-abo/swiper/issues/86
         )
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))
