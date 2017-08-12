;;; Slime

(defun $define-cl-indents ()
  ;; For `:default-initargs', see:
  ;; http://article.gmane.org/gmane.lisp.slime.devel/9814
  ;; http://article.gmane.org/gmane.lisp.slime.devel/9818
  (mapc 'define-cl-indent
        '((:default-initargs (&rest))
          (defpackage 1)
          (with-standard-io-syntax . progn))))

(defun $slime-load-hook ()
  (setq slime-net-coding-system 'utf-8-unix
        slime-lisp-implementations '((sbcl ("sbcl" "--noinform"))
                                     (ccl ("ccl"))
                                     (cmucl ("cmucl"))
                                     (clisp ("clisp" "-q" "-I"))))
  (add-hook 'cl-indent:load-hook '$define-cl-indents)
  (setq slime-asdf-collect-notes nil))

(req-package slime
  :init
  ;; * slime-fancy
  ;;
  ;;   a meta package which loads a combination of the most popular
  ;;   packages.
  (setq slime-contribs '(slime-fancy slime-asdf slime-indentation))
  (add-hook 'slime-load-hook '$slime-load-hook))
