;; cnfonts
;;
;; [[https://github.com/tumashu/cnfonts]]
;; [[http://www.newsmth.net/nForum/#!article/Emacs/108473]]
;; [[http://baohaojun.github.io/perfect-emacs-chinese-font.html]]
;; [[http://zhuoqiang.me/torture-emacs.html]]

(setup (:package cnfonts)

  (add-hook 'emacs-startup-hook 'cnfonts-mode)

  (:when-loaded

    (setopt cnfonts-personal-fontnames
            '(
              ;; 英文字体
              ("Sarasa Fixed SC"
               "Noto Sans Mono CJK SC" "Noto Sans Mono"
               "Droid Sans Fallback" "DejaVu Sans Mono")
              ;; 中文字体
              ("Sarasa Fixed SC"
               "Noto Sans Mono CJK SC" "Droid Sans Fallback"
               "WenQuanYi Micro Hei Mono" "WenQuanYi Micro Hei")
              ;; EXT-B 字体
              ("Jigmo2" "HanaMinB"
               "Hanazono Mincho" "Hanazono Mincho A" "Hanazono Mincho B"
               "Hanazono Mincho C" "Hanazono Mincho Ex" "Hanazono Mincho Ex A1"
               "Hanazono Mincho Ex A2" "Hanazono Mincho Ex B" "Hanazono Mincho Ex C"
               "Hanazono Mincho I")
              ;; Symbol 字符字体
              ("Sarasa Fixed SC" "Noto Sans Mono CJK SC"
               "Noto Color Emoji" "Noto Emoji"
               "OpenMoji" "Symbola")
              ;; Emacs 社区配置中，用于装饰的字符使用的字体
              ("Sarasa Fixed SC" "Noto Sans Mono CJK SC")
              )
            )

    (setopt cnfonts-profiles '("default"))

    (setopt cnfonts-use-face-font-rescale
            (cl-case system-type
              ((windows-nt ms-dos cygwin) nil)
              (t nil)
              ;; Can't use rescale on Debian.
              ))
    ;; Use different font size for different title, e.g., in org-mode.
    ;;
    ;; NOTE: This feature doesn't work on Windows.  It should work on most Linux
    ;; distributions.  It has not been tested on masOS.
    ;;
    ;; https://github.com/tumashu/cnfonts#cnfonts-%E4%B8%8E-org-mode-%E9%85%8D%E5%90%88%E4%BD%BF%E7%94%A8

    (setopt cnfonts-use-system-type t)
    ;; Put the profile into an system dependent path.
    ;;
    ;; This makes cnfonts use different configuration on different system for
    ;; each profile.

    )

  )
