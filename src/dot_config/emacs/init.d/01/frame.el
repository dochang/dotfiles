;;; Font width test
;;;
;;; 01234567890123456789012345678901234567890123456789012345678901234567890123456789
;;; 零一二三四五六七八九零一二三四五六七八九零一二三四五六七八九零一二三四五六七八九
;;; あいうえおアイウエオあいうえおアイウエオあいうえおアイウエオあいうえおアイウエオ
;;; 영일이삼사오육칠팔구영일이삼사오육칠팔구영일이삼사오육칠팔구영일이삼사오육칠팔구
;;;
;;; https://github.com/tumashu/cnfonts/blob/v1.1.1/cnfonts-ui.el#L408-L425
;;;
;;; | More haste, less speed. |
;;; | 为天地立心，为生民立命；|
;;; | 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀。|
;;; > αβχδεφγηιϕκλνοπθρστυʌɯʊ <
;;; > ❶➓Ⓐ⓾⊕⊡←∀



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
    (unless (bound-and-true-p cnfonts-mode)
      ;; Unless `cnfonts' is installed and activated, run the following setup.
      (set-frame-font "Sarasa Fixed SC" t (list frame))
      (dolist (script '(han kana hangul cjk-misc bopomofo gb18030))
        (set-fontset-font nil script "Sarasa Fixed SC" frame))
      (dolist (script '(hangul nil))
        (set-fontset-font nil script "Jigmo2" frame 'append))
      (dolist (script '(symbol phonetic))
        (set-fontset-font nil script "Sarasa Fixed SC" frame 'append)))))


(setup frame
  (add-hook 'emacs-startup-hook (lambda () (blink-cursor-mode -1)))
  (add-hook 'after-make-frame-functions #'$set-font))
