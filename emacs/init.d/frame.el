;;; Font width test
;;;
;;; 01234567890123456789012345678901234567890123456789012345678901234567890123456789
;;; 零一二三四五六七八九零一二三四五六七八九零一二三四五六七八九零一二三四五六七八九


;;; Font Configuration
(defun $set-font (&optional frame)
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  ;; When `after-make-frame-functions' runs this function, the `frame'
  ;; parameter is not `(selected-frame)'.  Because when
  ;; `after-make-frame-functions' running, the X window manager has not
  ;; switched the focus to the new Emacs frame.  At that time,
  ;; `(selected-frame)' is still the old frame, and `frame' is the new frame.
  (when (display-graphic-p frame)
    (unless (featurep 'cnfonts)
      ;; If `cnfonts' is not installed, run the following setup.
      ;;
      ;; `face-font-rescale-alist' has to be set when setting the font
      ;; configuration.
      ;;
      ;; Setting it during loading `.emacs' raises the following error if Emacs
      ;; is running in daemon mode or with `-nw' option:
      ;;
      ;;     set-face-attribute: Font not available: #<font-spec nil nil nil nil nil nil nil nil nil nil nil nil nil>
      ;;
      (add-to-list 'face-font-rescale-alist '("-Droid Sans Fallback-" . 1.25))
      (add-to-list 'face-font-rescale-alist '("-WenQuanYi Micro Hei Mono-" . 1.25))
      (add-to-list 'face-font-rescale-alist '("-文泉驿等宽微米黑-" . 1.25))
      (set-frame-font "DejaVu Sans Mono-11" t (list frame))
      (dolist (script '(han kana hangul symbol cjk-misc bopomofo))
        ;; There're no hangul characters in `fonts-droid` on Debian.  Use
        ;; `ttf-wqy-microhei` instead.
        (set-fontset-font nil script "WenQuanYi Micro Hei Mono-11" frame)))))


(req-package frame
  :ensure nil
  :hook (emacs-startup . (lambda () (blink-cursor-mode -1)))
  :init
  (add-hook 'after-make-frame-functions '$set-font))
