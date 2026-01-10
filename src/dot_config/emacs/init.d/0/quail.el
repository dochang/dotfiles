;; -*- lexical-binding: t; -*-

(setup quail

  (setq quail-japanese-use-double-n nil)
  ;; Force to not use `nn'.

  (with-eval-after-load 'hangul

    ;; https://en.wikipedia.org/wiki/Hangul_alphabetical_order
    ;; ㄱ ㄲ ㄴ ㄷ ㄸ ㄹ ㅁ ㅂ ㅃ ㅅ ㅆ ㅇ ㅈ ㅉ ㅊ ㅋ ㅌ ㅍ ㅎ
    ;; ㅏ ㅐ ㅑ ㅒ ㅓ ㅔ ㅕ ㅖ ㅗ ㅘ ㅙ ㅚ ㅛ ㅜ ㅝ ㅞ ㅟ ㅠ ㅡ ㅢ ㅣ
    ;; ∅ ㄱ ㄲ ㄳ ㄴ ㄵ ㄶ ㄷ ㄹ ㄺ ㄻ ㄼ ㄽ ㄾ ㄿ ㅀ ㅁ ㅂ ㅄ ㅅ ㅆ ㅇ ㅈ ㅊ ㅋ ㅌ ㅍ ㅎ

    (let* ((jaum-count 30)
           ;; count of jaum
           (moum-count 21)
           ;; count of moum
           (jamo-code (lambda (jamo) (+ jamo #x3130)))
           ;; Jamo index starts from 1.
           (hangul-jamo-table
            ;; Useless?
            ;; ㄱㄲㄳㄴㄵㄶㄷㄸㄹㄺㄻㄼㄽㄾㄿㅀㅁㅂㅃㅄㅅㅆㅇㅈㅉㅊㅋㅌㅍㅎㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ
            (let* ((s (make-string 51 #x3131))
                   (l (length s)))
              (dotimes (i l s)
                (aset s i (funcall jamo-code (1+ i))))))
           (hangul2-djamo-keymap
            (let ((map (list)))
              (dolist (pair hangul-djamo-table map)
                (dolist (pair2 (cdr pair))
                  (let* ((base (car pair2))
                         (vec (cdr pair2))
                         (l (length vec)))
                    (dotimes (i l)
                      (setq map
                            (cons (cons (+ base 1 i)
                                        (vector base (aref vec i)))
                                  map)))))))))
      (define-advice hangul-input-method-help (:override (&rest r) show-key-sequences)
        "Complete the documentation of the hangul input method.

Add key sequences."
        (let (last-point
              (orig-help-text hangul-input-method-help-text)
              (orig-input-method-function input-method-function))
          (with-temp-buffer

            (insert "

KEY SEQUENCE
------------
You can input characters by the following key sequences:
char key  [type a key sequence to insert the corresponding character]
")

            (pcase orig-input-method-function
              ('hangul2-input-method
               (insert "
Choseong/Jongseong               Jungseong
--  --  --                       --  --
")
               (let* ((width 32)
                      (insert-single-hint
                       (lambda (jamo &optional upcase)
                         (insert (+ (if upcase ?A ?a)
                                    (seq-position hangul2-keymap jamo)))))
                      (insert-hint
                       (lambda (jamo)
                         (insert (funcall jamo-code jamo))
                         (insert "  ")
                         (if-let* ((dbl-idx (cdr (assoc jamo hangul2-djamo-keymap))))
                             (progn
                               (mapc insert-single-hint dbl-idx)
                               (when (= (aref dbl-idx 0)
                                        (aref dbl-idx 1))
                                 (insert "  ")
                                 (funcall insert-single-hint (aref dbl-idx 0) 'upcase)))
                           (if (or (= jamo 34) (= jamo 38))
                               ;; #x3152 ㅒ & #x3156 ㅖ
                               (funcall insert-single-hint (- jamo 2) 'upcase)
                             (funcall insert-single-hint jamo))))))
                 (dotimes (i jaum-count)
                   (setq last-point (point))
                   (funcall insert-hint (1+ i))

                   (when (< i moum-count)
                     (insert (make-string (- width (- (point) last-point)) ? ))
                     (funcall insert-hint (+ i jaum-count 1)))

                   (newline))))

              ((or 'hangul3-input-method 'hangul390-input-method)
               (insert "
Choseong         Jungseong        Jongseong        ASCII
--  --           --  --           --  --           --  --
")
               (let* ((width 16)
                      jung-width
                      (insert-hint
                       (lambda (insert-fn jamo)
                         (insert (funcall jamo-code jamo))
                         (insert "  ")
                         (if-let* ((dbl-idx (cdr (assoc jamo hangul2-djamo-keymap))))
                             (mapc insert-fn dbl-idx)
                           (funcall insert-fn jamo))))
                      (insert-lines
                       (lambda (insert-hint-cho
                                insert-hint-jung
                                insert-hint-jong
                                insert-hint-other
                                other-char-table)
                         (dotimes (i jaum-count)
                           (setq last-point (point))
                           (funcall insert-hint insert-hint-cho (1+ i))

                           (insert (make-string (- width (- (point) last-point)) ? ))
                           (setq last-point (point))
                           (setq jung-width width)
                           (if (< i moum-count)
                               (funcall insert-hint insert-hint-jung (+ i jaum-count 1))
                             (setq jung-width (1+ jung-width))
                             ;; 1 hangul occupies 2 spaces.
                             (insert (make-string jung-width ? )))

                           (insert (make-string (- jung-width (- (point) last-point)) ? ))
                           (setq last-point (point))
                           (funcall insert-hint insert-hint-jong (1+ i))

                           (insert (make-string (- width (- (point) last-point)) ? ))
                           (when (< i (length other-char-table))
                             (let ((char (aref other-char-table i)))
                               (insert char)
                               (insert "   ")
                               (funcall insert-hint-other char)))

                           (newline)))))
                 (pcase orig-input-method-function
                   ('hangul3-input-method
                    (let* ((insert-single-hint-fn
                            (lambda (fn)
                              (lambda (jamo)
                                (insert
                                 (+ 33 (seq-position hangul3-keymap
                                                     (funcall fn jamo)))))))
                           (insert-hint-cho
                            (funcall insert-single-hint-fn
                                     (lambda (jamo) (+ jamo 92))))
                           (insert-hint-jung
                            (funcall insert-single-hint-fn
                                     (lambda (jamo) (+ jamo 35))))
                           (insert-hint-jong
                            (funcall insert-single-hint-fn #'identity))
                           (insert-hint-other
                            (funcall insert-single-hint-fn #'identity)))
                      (funcall insert-lines
                               insert-hint-cho
                               insert-hint-jung
                               insert-hint-jong
                               insert-hint-other
                               "·“'~”)4,>.!?07123\"-8965(:<=;*%\\/※")))
                   ('hangul390-input-method
                    (let* ((insert-single-hint-fn
                            (lambda (fn)
                              (lambda (jamo)
                                (insert
                                 (+ 33 (seq-position hangul390-keymap
                                                     (funcall fn jamo)))))))
                           (insert-hint-cho
                            (funcall insert-single-hint-fn
                                     (lambda (jamo)
                                       (+ jamo
                                          (if (< jamo 7)
                                              ;; #x3131 ㄱ & #x3134 ㄴ
                                              86 92)))))
                           (insert-hint-jung
                            (funcall insert-single-hint-fn
                                     (lambda (jamo) (+ jamo 34))))
                           (insert-hint-jong
                            (funcall insert-single-hint-fn #'identity))
                           (insert-hint-other
                            (funcall insert-single-hint-fn #'identity)))
                      (funcall insert-lines
                               insert-hint-cho
                               insert-hint-jung
                               insert-hint-jong
                               insert-hint-other
                               "!'/0123456789;<>"))))))

              (_ (erase-buffer)))

            (let ((help-text (buffer-string)))
              (with-output-to-temp-buffer "*Help*"
                (princ orig-help-text)
                (princ help-text)))))))

    )

  (let ((load-dirs (locate-user-emacs-file "im")))
    (load-dirs))

  )
