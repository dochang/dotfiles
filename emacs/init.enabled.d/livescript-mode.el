;;; LiveScript Mode
;; There're 3 versions of livescript-mode.
;;
;; - [[https://github.com/tensai-cirno/livescript-mode]]
;; - [[https://github.com/yhisamatsu/livescript-mode]]
;; - [[https://github.com/bdowning/livescript-mode]]
;;
;; Here we use the one from yhisamatsu.
;;
;; See [[https://github.com/gkz/LiveScript/wiki/Projects-supporting-LiveScript#editor-support]]

(defun $livescript-mode-hook ()
  ($run-prog-mode-hook)
  ($camel-case-mode 1))

(req-package livescript-mode
  :mode ("Slakefile\\'" . livescript-mode)
  :init
  (add-hook 'livescript-mode-hook '$livescript-mode-hook))
