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
  :hook (emacs-startup . cnfonts-mode)
  :init
  (setq cnfonts-personal-fontnames
        '(
          ;; 英文字体
          ("Noto Sans Mono CJK" "Noto Sans Mono" "Droid Sans Fallback"
           "DejaVu Sans Mono" "Liberation Mono")
          ;; 中文字体
          ("Noto Sans Mono CJK SC" "Noto Sans Mono CJK TC"
           "Noto Sans Mono CJK JP" "Noto Sans Mono CJK KR"
           "WenQuanYi Micro Hei Mono" "WenQuanYi Micro Hei"
           "HanaMinA" "Droid Sans Fallback")
          ;; EXT-B 字体
          ("HanaMinB"
           "Hanazono Mincho" "Hanazono Mincho A" "Hanazono Mincho B"
           "Hanazono Mincho C" "Hanazono Mincho Ex" "Hanazono Mincho Ex A1"
           "Hanazono Mincho Ex A2" "Hanazono Mincho Ex B" "Hanazono Mincho Ex C"
           "Hanazono Mincho I")
          ;; Symbol 字符字体
          ("Noto Sans Mono CJK SC" "Noto Sans Mono CJK TC"
           "Noto Sans Mono CJK JP" "Noto Sans Mono CJK KR"
           "Noto Color Emoji" "Noto Emoji" "OpenMoji" "Symbola")
          ;; Emacs 社区配置中，用于装饰的字符使用的字体
          ("Noto Sans Mono CJK SC" "Noto Sans Mono CJK TC"
           "Noto Sans Mono CJK JP" "Noto Sans Mono CJK KR")
          )
        )
  (setq cnfonts-profiles '("default"))
  ;; Use different font size for different title, e.g., in org-mode.
  ;;
  ;; NOTE: This feature doesn't work on Windows.  It should work on most Linux
  ;; distributions.  It has not been tested on masOS.
  ;;
  ;; https://github.com/tumashu/cnfonts#cnfonts-%E4%B8%8E-org-mode-%E9%85%8D%E5%90%88%E4%BD%BF%E7%94%A8
  (setq cnfonts-use-face-font-rescale
        (cl-case system-type
          ((windows-nt ms-dos cygwin) nil)
          (t t)))
  ;; Put the profile into an system dependent path.
  ;;
  ;; This makes cnfonts use different configuration on different system for
  ;; each profile.
  (setq cnfonts-use-system-type t))
