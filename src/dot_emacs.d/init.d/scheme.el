;;; Scheme Mode


;; FIXME this duplicates almost all of lisp-indent-function.
;; Extract common code to a subroutine.
(defun $scheme-indent-function (indent-point state)
  "Scheme mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `scheme-indent-function'
\(or the deprecated `scheme-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (and (> (point) 1)
                    (save-excursion
                      (backward-char)
                      (looking-at "#:")))))
      ;; car of form doesn't seem to be a symbol, or is a keyword
      (progn
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column)))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (and (> (point) 1)
                       (save-excursion
                         (backward-char)
                         (looking-at "#:")))))
           (save-excursion
             (goto-char orig-point)
             (and (> (point) 1)
                  (save-excursion
                    (backward-char)
                    (looking-at "#:")))))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'scheme-indent-function)
                         (get (intern-soft function) 'scheme-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent))))))))

(defun $scheme-mode-hook ()
  ($lisp-mode-common-hook)
  (setq-local lisp-indent-function '$scheme-indent-function))

(req-package scheme
  :ensure (scheme :pin :built-in)

  :hook (scheme-mode . $scheme-mode-hook))
