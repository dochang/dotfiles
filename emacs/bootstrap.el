(load (locate-user-emacs-file "packages"))

(unless (require 'el-get nil 'noerror)
  (with-temp-buffer
    (with-current-buffer
        (let* ((url "https://raw.github.com/dimitri/el-get/master/el-get-install.el"))
          (cond ((executable-find "curl")
                 (call-process "curl" nil t nil "--silent" "--show-error" "--location" url)
                 (current-buffer))
                ((executable-find "wget")
                 (call-process "wget" nil t nil "--quiet" "--output-document" "-" url)
                 (current-buffer))
                (t
                 (url-retrieve-synchronously url))))
      (let (el-get-master-branch
            el-get-install-skip-emacswiki-recipes
            (el-get-git-install-url (getenv "EL_GET_GIT_INSTALL_URL")))
        (when (and (stringp el-get-git-install-url)
                   (string= "" el-get-git-install-url))
          (setq el-get-git-install-url nil))
        (goto-char (point-max))
        (eval-print-last-sexp)))))

(unless (require 'package nil 'noerror)
  (el-get 'sync 'package))

(setq package-enable-at-startup nil)
(require 'dired-x nil 'noerror)
;; `dired+' reads the value of `dired-omit-files' on initializing.  This
;; variable is defined in `dired-x'.  So `dired-x' must be loaded right before
;; `package-initialize'.
;;
;; https://github.com/emacsmirror/emacswiki.org/blob/a7f52e3fbd07765dc3042a1799b2310bfc88663b/dired%2B.el#L2195
(package-initialize)

;; The URL package now supports HTTPS over proxies supporting CONNECT.  We
;; don't need mb-url any more.
(when (version< emacs-version "26")

  (or (require 'mb-url nil 'noerror)
      (el-get 'sync 'mb-url)
      (el-get-bundle! mb-url in dochang/mb-url
        :branch "master"
        :depends ()
        :library mb-url))

  (setq mb-url-http-backend 'mb-url-http-curl)
  (advice-add 'url-http :around 'mb-url-http-around-advice)

  (unless (package-installed-p 'mb-url)
    (unless (assq 'mb-url package-archive-contents)
      (package-refresh-contents))
    (package-install 'mb-url))
  ;; If mb-url has not been installed, I believe the archive contents are empty.
  ;; If mb-url cannot be installed, clear the archive.

  ;; Delete el-get bootstrap mb-url
  (when (el-get-package-installed-p 'mb-url)
    (el-get-remove 'mb-url)))
