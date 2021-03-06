;;; CC Mode
(defun $c-initialization-hook ()
  (smart-tabs-insinuate 'c 'c++ 'java))

(defun $c-mode-common-hook ()
  ($run-prog-mode-hook)
  ;; Fix the indentation for anonymous class in java-mode.
  ;;
  ;; [[https://stackoverflow.com/a/7619497]]
  ;; [[http://www.mail-archive.com/jde@sunsite.auc.dk/msg01159.html]]
  (c-set-offset 'substatement-open 0)
  (when (assoc 'inexpr-class c-offsets-alist)
    (c-set-offset 'inexpr-class 0))
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0))

(defun $java-mode-hook ()
  ($camel-case-mode 1))

(req-package cc-mode
  :hook ((c-initialization . $c-initialization-hook)
         (c-mode-common . $c-mode-common-hook)
         (java-mode . $java-mode-hook)))
