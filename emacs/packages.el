

;;; Packages
(defvar **package-archives** (bound-and-true-p package-archives))

(setq package-archives
      '(
{% for archive in dotfiles_emacs_package_archives %}
        ({{ archive.id | to_json }} . {{ archive.location | to_json }})
{% endfor %}
        ))


;;; el-get
;; [[https://github.com/dimitri/el-get]]
;; [[http://tapoueh.org/emacs/el-get.html]]
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(setq el-get-is-lazy t)

(setq el-get-github-default-url-type 'https)

(setq el-get-byte-compile-at-init nil)

(setq el-get-byte-compile nil)

(defvar **packages**
  '(emacs-http-server
    ;;simple-server
    elnode
    blorg
    o-blog))

;; Make sure that `**custom-themes**' and `**color-themes**' are not void
;; during `el-get`.
(defvar **custom-themes** '())
(defvar **color-themes** '())

;; ## Load order of options in recipe.
;;
;; 1. byte-compile
;; 2. autoloads
;; 3. prepare
;; 4. before
;; 5. load
;; 6. features
;; 7. post-init
;; 8. <source code>
;; 9. after
;;
;; If `:lazy' is true:
;;
;;   - 5 - 6 will be skipped;
;;   - 7 - 9 will be "eval-after-load".

(setq el-get-sources
      `(;; coffee-mode has configured `auto-mode-alist' in autoload code.  Do
        ;; not do it in `:post-init' again.  Also, `coffee-js-mode` has been
        ;; removed.  So delete all `:post-init' code.
        (:name coffee-mode
         :post-init nil)
        ;; Do not edit `*.hs' and `*.handlebars' with `mustache-mode'.
        (:name mustache-mode
         :post-init nil)
        ;; Do not call `flymake-ruby-load'.  It would be called only if
        ;; flycheck doesn't exist.
        (:name flymake-ruby
         :post-init nil)
        ;; Do not call `flymake-less-load'.  It would be called only if
        ;; flycheck doesn't exist.
        ;;
        ;; Install dependencies via `package'
        (:name flymake-less
         :depends ()
         :post-init nil)
        ;; Do not call `flymake-coffee-load'.  It would be called only if
        ;; flycheck doesn't exist.
        (:name flymake-coffee
         :post-init nil)
        ;; Do not call `flymake-shell-load'.  It would be called only if
        ;; flycheck doesn't exist.
        (:name flymake-shell
         :post-init nil)
        ;; Do not call `flymake-css-load'.  It would be called only if flycheck
        ;; doesn't exist.
        (:name flymake-css
         :post-init nil)
        ;; Add website information.
        (:name textile-mode
         :website "https://github.com/juba/textile-mode")
        ;; ri is deprecated.  Use yari instead.
        (:name ri
         :website "https://raw.github.com/emacsmirror/emacswiki.org/master/ri.el"
         :description "Ruby Documentation Lookup"
         :type emacswiki)
        (:name color-theme-hober2
         :website "http://edward.oconnor.cx/config/elisp/color-theme-hober2.el"
         :description "Edward O'Connor's second color theme."
         ;; :depends (color-theme)
         :type http
         :url "http://edward.oconnor.cx/config/elisp/color-theme-hober2.el")
        (:name hober2-theme
         :website "http://edward.oconnor.cx/config/elisp/hober2-theme.el"
         :description "Edward O'Connor's second theme."
         :type http
         :url "http://edward.oconnor.cx/config/elisp/hober2-theme.el"
         :autoloads nil
         :compile ()
         :prepare (when (boundp 'custom-theme-load-path)
                    (add-to-list 'custom-theme-load-path default-directory)))
        (:name color-theme-tango
         :depends ()              ; color-theme
         :website "http://www.emacswiki.org/emacs/color-theme-tango.el")
        ;; Edit download url to avoid conflict with zenburn-theme on MELPA.
        ;; Install this will only install the color theme version.  Install the
        ;; custom theme version from MELPA.
        (:name color-theme-zenburn
         :website "https://github.com/bbatsov/zenburn-emacs/tree/0c46ca823dd007241c48778d38b80ac8bde6d5ee"
         :description "A low contrast color theme for Emacs."
         ;; :depends (color-theme)
         :type http
         :url "https://raw.github.com/bbatsov/zenburn-emacs/0c46ca823dd007241c48778d38b80ac8bde6d5ee/color-theme-zenburn.el"
         :prepare (autoload 'color-theme-zenburn "color-theme-zenburn" nil t))
        (:name groovy-emacs-mode
         :library groovy-mode)
        (:name dictionary
         :lazy nil
         :autoloads "dictionary-init"
         :features dictionary)
        ;; `erlang-mode.el' at [1] is too old.  The most up-to-date code is
        ;; in Erlang's code repo, which is too big to download.  Use the
        ;; version from MELPA instead.
        ;;
        ;; [1] http://www.erlang.org/download/contrib/erlang.el
        (:name erlang-mode
         :website "https://melpa.org/#/erlang"
         :description "Major mode for editing and running Erlang"
         :type elpa)
        (:name elixir
         :library elixir-mode)
        ;; Install dependencies via `package'
        (:name elm-mode
         :depends ())
        ;; Install dependencies via `package'
        (:name pip-requirements
         :depends ())
        (:name httprepl
         :library httprepl)
        (:name toml-mode
         :library toml-mode)
        (:name haskell-mode
         :post-init nil)
        ;; Install dependencies via `package'
        (:name cnfonts
         :depends ())
        (:name noflet
         :library noflet)
        ;; Install my version because of [[https://github.com/nicferrier/curl-url-retrieve/pull/2]]
        ;;
        ;; Once this PR merged, add recipes to MELPA & el-get.
        (:name curl-url-retrieve
         :website "https://github.com/nicferrier/curl-url-retrieve"
         :description "use url-retrieve with curl doing the work"
         :depends (noflet)
         :type github
         :pkgname "dochang/curl-url-retrieve"
         :library curl-for-url)
        (:name taskjuggler-mode
         :website "http://www.skamphausen.de/cgi-bin/ska/taskjuggler-mode"
         :description "Editing Taskjuggler Files"
         :type http
         :url "http://www.skamphausen.de/cgi-bin/ska/download/taskjuggler-mode.el"
         :prepare (progn
                    (autoload 'taskjuggler-mode "taskjuggler-mode" nil t)
                    (add-to-list 'auto-mode-alist '("\\.tjp\\'" . taskjuggler-mode))
                    (add-to-list 'auto-mode-alist '("\\.tji\\'" . taskjuggler-mode))
                    (add-to-list 'auto-mode-alist '("\\.tjsp\\'" . taskjuggler-mode))))
        ;; 1. Do not build info documents or it reports an error.
        ;;
        ;;
        ;; 2. Do not byte compile `contrib/*.el', because el-get cannot specify
        ;; the order.
        ;;
        ;; [[http://article.gmane.org/gmane.lisp.slime.devel/10731]]
        ;; [[http://lists.common-lisp.net/pipermail/slime-devel/2012-February/018470.html]]
        ;; [[https://github.com/pallet/ritz/issues/60]]
        ;; [[https://bugs.launchpad.net/slime/+bug/1027361]]
        ;; [[https://launchpadlibrarian.net/110774405/slime-byte-compile.patch]]
        ;; [[http://debbugs.gnu.org/cgi/bugreport.cgi?bug=11821]]
        (:name slime
         :compile ("\\.el\\'")
         :info nil
         :build nil
         :build/berkeley-unix nil)))

(when (boundp 'package-user-dir)
  (unless (file-exists-p package-user-dir)
    (make-directory package-user-dir t)))
