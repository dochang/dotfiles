(defvar use-package-el-get-bundle-keyword :el-get-bundle)

;; Insert `:el-get-bundle' keyword after `:unless' so that el-get-bundle only
;; runs if either `:if', `:when', `:unless' or `:requires' are satisfied
(defun use-package-el-get-bundle-set-keyword ()
  (unless (member use-package-el-get-bundle-keyword use-package-keywords)
    (setq use-package-keywords
          (let* ((pos (cl-position :unless use-package-keywords))
                 (head (cl-subseq use-package-keywords 0 (+ 1 pos)))
                 (tail (nthcdr (+ 1 pos) use-package-keywords)))
            (append head (list use-package-el-get-bundle-keyword) tail)))))

(defun use-package-normalize/:el-get-bundle (name-symbol keyword args)
  "use-package :el-get-bundle keyword handler"
  (let ((arg (car args)))
    (pcase arg
      ((or `nil `t) (list name-symbol))
      ((pred symbolp) args)
      ((pred consp)
       (cond
        ((string-match "^:" (symbol-name (car arg)))
         (cons name-symbol arg))
        ((symbolp (car arg)) args)))
      (_ (use-package-error
          ":el-get-bundle wants a package name or boolean value or an el-get-bundle recipe")))))

(defun use-package-handler/:el-get-bundle (name-symbol keyword args rest state)
  "use-package :el-get-bundle keyword handler"
  (let ((body (use-package-process-keywords name-symbol rest state)))
    ;; This happens at macro expansion time, not when the expanded code is
    ;; compiled or evaluated.
    (if args
        (use-package-concat
         `((unless (package-installed-p ',name-symbol)
             ;; DO NOT check `el-get-package-installed-p', always call
             ;; `el-get-bundle'.  `(el-get)' requires the local recipe even if
             ;; the package has been installed.
             (el-get-bundle ,@(car args))))
         body)
      body)))

(define-advice use-package-handler/:ensure
    (:around (fn name-symbol keyword ensure rest &rest r) el-get-bundle-override-:ensure)
  (apply fn
         name-symbol
         keyword
         (if (plist-member rest :el-get-bundle)
             nil
           ensure)
         rest
         r))

(use-package-el-get-bundle-set-keyword)
