(with-eval-after-load 'mb-url-http
  (setq mb-url-http-backend 'mb-url-http-curl)
  (advice-add 'url-http :around 'mb-url-http-around-advice))

(require 'package)
(declare-function package-installed-p "ext:package")

(if (package-installed-p 'mb-url)
    (require 'mb-url-http)
  (let ((mb-url-dir (expand-file-name (locate-user-emacs-file "bootstrap.d/mb-url"))))
    (unless (file-directory-p mb-url-dir)
      (call-process "git" nil (get-buffer-create "bootstrap mb-url") nil "clone" "https://github.com/dochang/mb-url" mb-url-dir))
    (setq load-path (append load-path (list mb-url-dir)))
    (require 'mb-url-http)
    (package-refresh-contents)
    (package-install 'mb-url)
    (if (file-symlink-p mb-url-dir)
        (delete-file mb-url-dir)
      (delete-directory mb-url-dir 'recursive))))

(defun $ensure-require (feature &optional package)
  "Ensure PACKAGE installed before require FEATURE."
  (setq package (or package feature))
  (unless (featurep feature)
    (unless (package-installed-p package)
      (package-install package))
    (require feature)))

;; https://github.com/quelpa/quelpa
(setq quelpa-update-melpa-p nil)
(setq quelpa-checkout-melpa-p nil)
(setq quelpa-upgrade-p nil)
(setq quelpa-self-upgrade-p t)
(setq quelpa-upgrade-interval 7)
;; Default value is nil, which means no upgrade.
(setq quelpa-autoremove-p t)
;; Force to autoremove old package versions
($ensure-require 'quelpa)

;; https://github.com/dimitri/el-get
;; https://www.emacswiki.org/emacs/el-get
($ensure-require 'el-get)

;; https://github.com/jwiegley/use-package
;; https://www.emacswiki.org/emacs/UsePackage
($ensure-require 'use-package)

;; https://github.com/conao3/leaf.el
($ensure-require 'leaf)

;; https://git.sr.ht/~pkal/setup
;; https://www.emacswiki.org/emacs/SetupEl
($ensure-require 'setup)

(setup-define :quelpa
  (lambda (arg &rest plist)
    (when (eq 'quote (car arg))
      (setq arg (cadr arg)))
    (let ((rcp (quelpa-arg-rcp arg)))
      `(unless (or (package-installed-p ',(car rcp))
                   (package-built-in-p ',(car rcp)))
         (quelpa-package-install ',rcp ,@plist))))
  :documentation "Install PACKAGE by quelpa if it hasn't been installed yet.
The first PACKAGE can be used to deduce the feature context."
  :repeatable t
  :shorthand
  (lambda (head)
    (let ((arg (cadr head)))
      (when (eq 'quote (car arg))
        (setq arg (cadr arg)))
      (car (quelpa-arg-rcp arg)))))

(setup-define :el-get-bundle
  (lambda (package &rest form)
    `(el-get-bundle ,package ,@form))
  :documentation "Install PACKAGE by el-get-bundle if it hasn't been installed yet.
The first PACKAGE can be used to deduce the feature context."
  :repeatable t)

(setup-define :el-get-bundle!
  (lambda (package &rest form)
    `(el-get-bundle! ,package ,@form))
  :documentation "Install PACKAGE by el-get-bundle! if it hasn't been installed yet.
The first PACKAGE can be used to deduce the feature context."
  :repeatable t
  :shorthand #'cadr)
