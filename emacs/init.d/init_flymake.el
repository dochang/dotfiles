;; local::lib support for perl flymake.
(defcustom perl-local-lib-path nil
  "The directory used by local::lib."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Default location" t)
                 (directory :tag "Directory"))
  :group 'perl
  :safe (lambda (value) (or (booleanp value) (stringp value))))

(define-advice flymake-perl-init (:filter-return (retval) local-lib-support)
  "local::lib support."
  ;; Make local variables take effect first.
  (hack-local-variables)
  (let* ((args (nth 1 retval))
         (mod-exist (and (executable-find "perl")
                         ;; Just test the existence of `local::lib`.  Do not
                         ;; creating the local dir here.
                         (zerop (call-process "perl" nil nil nil
                                              "-mlocal::lib" "-e" "1"))))
         (local-lib (cond ((not mod-exist) nil)
                          ((stringp perl-local-lib-path)
                           (format "-Mlocal::lib=%s" perl-local-lib-path))
                          (perl-local-lib-path "-Mlocal::lib"))))
    (when local-lib
      (setf (nth 1 retval) (cons local-lib args)))))

;; Flymake for Lua
(defun $flymake-lua-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "luac" (list "-p" local-file))))

(req-package flymake
  :loader :built-in
  :init
  (setq flymake-gui-warnings-enabled nil)
  ;; Log messages with level <= 1 (WARNING)
  (setq flymake-log-level 1)
  :config
  (add-to-list 'flymake-allowed-file-name-masks
               '("Rexfile\\'" flymake-perl-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.lua\\'" $flymake-lua-init))
  (add-to-list 'flymake-err-line-patterns
               '("^.*luac[0-9.]*\\(.exe\\)?: *\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 2 3 nil 4)))
