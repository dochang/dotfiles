(req-package ivy
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-partial)
         ;; Make TAB only complete and do not open file or directory.
         ;;
         ;; https://github.com/abo-abo/swiper/issues/86
         )
  :init
  (setq ivy-display-style (if (or (daemonp) (display-graphic-p)) 'fancy nil))
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d/%d) "))
