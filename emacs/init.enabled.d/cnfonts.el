;; cnfonts
;;
;; [[https://github.com/tumashu/cnfonts]]
;; [[http://www.newsmth.net/nForum/#!article/Emacs/108473]]
;; [[http://baohaojun.github.io/perfect-emacs-chinese-font.html]]
;; [[http://zhuoqiang.me/torture-emacs.html]]
(req-package cnfonts
  :commands (cnfonts-edit-profile
             cnfonts-regenerate-profile
             cnfonts-switch-profile
             cnfonts-next-profile
             cnfonts-increase-fontsize
             cnfonts-decrease-fontsize)
  :init
  (setq cnfonts-profiles '("default"))
  ;; Use different font size for different title, e.g., in org-mode.
  ;;
  ;; NOTE: This feature doesn't work on Windows.  It should work on most Linux
  ;; distributions.  It has not been tested on masOS.
  (setq cnfonts-use-face-font-rescale t)
  ;; Put the profile into an system dependent path.
  ;;
  ;; This makes cnfonts use different configuration on different system for
  ;; each profile.
  (setq cnfonts-use-system-type t))
