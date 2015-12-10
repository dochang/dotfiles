

;;; Packages
(defvar **package-archives** (bound-and-true-p package-archives))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")

        ;; Downloading marmalade archive will cause Emacs hangs at 100% CPU
        ;; usage.  Disable it for now.  See nicferrier/elmarmalade#106 .
        ;;
        ;; ("marmalade" . "https://marmalade-repo.org/packages/")

        ("ELPA" . "http://tromey.com/elpa/")
        ("SC" . "http://joseito.republika.pl/sunrise-commander/")

        ))

(setq el-get-emacswiki-base-url "http://www.emacswiki.org/emacs/download/")


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
      `((:name vcard-mode
               :website "https://github.com/dochang/vcard-mode#readme"
               :description "A major mode to edit vCard files in Emacs"
               :type github
               :pkgname "dochang/vcard-mode"
               :before (add-to-list 'auto-mode-alist '("\\.vc\\(f\\|ard\\)\\'" . vcard-mode)))
        (:name lua2-mode
               :website "http://www.enyo.de/fw/software/lua-emacs/lua2-mode.html"
               :description "A semantic highlighting extension for lua-mode"
               ;; :depends (lua-mode)
               :type http
               :url "http://www.enyo.de/fw/software/lua-emacs/lua2-mode.el")
        (:name lua-block
               :website "https://raw.github.com/emacsmirror/emacswiki.org/master/lua-block.el"
               :description "highlight matching block"
               ;; :depends (lua-mode)
               :type emacswiki)
        (:name yaml-path
               :website "https://github.com/craig-ludington/yaml-path"
               :description "Extends yaml-mode to display the path of the current yaml element in the message area."
               :type github
               :pkgname "craig-ludington/yaml-path"
               :prepare (autoload 'yaml-path/path "yaml-path"
                          "Display the path to the current YAML element in the message area." t))
        ;; coffee-mode has configured `auto-mode-alist' in autoload code.  Do
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
        (:name ri
               :website "https://raw.github.com/emacsmirror/emacswiki.org/master/ri.el"
               :description "Ruby Documentation Lookup"
               :type emacswiki)
        ;; There are 2 versions.
        ;;
        ;;  1. [[https://github.com/ruby/ruby/blob/trunk/misc/rdoc-mode.el]]
        ;;  2. [[https://github.com/jwiegley/ruby-mode/blob/master/rdoc-mode.el]]
        ;;
        ;; 1 is newer than 2.
        (:name rdoc-mode
               :website "https://github.com/ruby/ruby/blob/trunk/misc/rdoc-mode.el"
               :description "Major mode for RDoc editing"
               :type http
               :url "https://raw.github.com/ruby/ruby/trunk/misc/rdoc-mode.el"
               :prepare (add-to-list 'auto-mode-alist '("\\.rdoc\\'" . rdoc-mode)))
        ;; There are 2 versions of `color-theme-blackboard'.
        ;;
        ;; 1. JD Huntington
        ;;
        ;;     [[http://blog.jdhuntington.com/2008/11/emacs-color-theme-blackboard.html]]
        ;;     [[http://jdhuntington.com/paste/color-theme-blackboard.el.html]]
        ;;
        ;; 2. Jason Lewis <jason@dickson.st>
        ;;
        ;;     [[https://github.com/jasonblewis/color-theme-blackboard]]
        ;;
        ;; The 2nd version just add a copyright information and a `provide' for
        ;; the 1st one.  We use it.
        (:name color-theme-blackboard
               :website "https://github.com/jasonblewis/color-theme-blackboard"
               :description "Blackboard Colour Theme for Emacs."
               ;; :depends (color-theme)
               :type github
               :pkgname "jasonblewis/color-theme-blackboard")
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
        ;; 1. Use package.el to install tangotango for Emacs 24.  Do not load
        ;; the theme from here.
        ;;
        ;; 2. Do not depend on color-theme, or el-get will install color-theme.
        ;; Instead, install color-theme by package.el
        (:name color-theme-tangotango
               :depends ()
               :autoloads nil
               :compile ())
        (:name color-theme-empty-void
               :website "http://www.emacswiki.org/emacs/color-theme-empty-void.el"
               :description "The Empty Void color theme, by mtvoid (based on sunburst)"
               ;; :depends (color-theme)
               :type emacswiki)
        ;; There are 2 versions on EmacsWiki.
        ;;
        ;; 1. [[http://www.emacswiki.org/emacs/ColorThemeInkpot]]
        ;; 2. [[http://www.emacswiki.org/emacs/color-theme-inkpot.el]]
        ;;
        ;; They're the same.  Version 2 just add description and copyright.
        (:name color-theme-inkpot
               :website "http://www.emacswiki.org/emacs/ColorThemeInkpot"
               :description "Color theme based on the Inkpot theme. Ported and tweaked by Per Vognsen."
               ;; :depends (color-theme)
               :type emacswiki)
        (:name color-theme-wombat
               :website "https://github.com/jasonblewis/color-theme-wombat"
               :description "Vim color theme ported to Emacs."
               ;; :depends (color-theme)
               :type github
               :pkgname "jasonblewis/color-theme-wombat")
        ;; This theme is a fork of `color-theme-wombat+`.  We won't use the
        ;; original fork since the site is too slow.
        ;;
        ;; [[http://jaderholm.com/color-themes/color-theme-wombat+.el]]
        ;; [[http://jaderholm.com/color-themes/color-theme-wombat+.el.sept2013]]
        (:name color-theme-wombat-dark
               :website "https://github.com/leoncamel/color-theme-wombat-dark"
               :description "wombat with improvements and many more faces"
               ;; :depends (color-theme)
               :type github
               :pkgname "leoncamel/color-theme-wombat-dark")
        ;; There are 5 versions of Color Theme Zenburn.
        ;;
        ;; [[http://www.emacswiki.org/emacs/ColorThemeZenburn]]
        ;;
        ;; 1. [[https://github.com/bbatsov/zenburn-emacs]]
        ;; 2. [[https://github.com/djcb/elisp/blob/master/themes/zenburn-theme.el]]
        ;; 3. [[http://www.emacswiki.org/emacs/zenburn.el]]
        ;; 4. [[https://github.com/dbrock/zenburn-el]]
        ;; 5. [[https://github.com/bbatsov/zenburn-emacs/blob/0c46ca823dd007241c48778d38b80ac8bde6d5ee/color-theme-zenburn.el]]
        ;;
        ;;
        ;; - 1-2 are for custom theme.
        ;; - 4-5 are for color theme.
        ;; - 2 is too old.
        ;; - 3 is an old version of 1.
        ;; - 4 supports more packages than 5.
        ;;
        ;; Conclusion: use 1 & 4.
        ;;
        ;; Installation methods:
        ;;
        ;; - For custom theme: (package-install 'zenburn-theme)
        ;; - For color theme: (el-get-install 'zenburn)
        (:name zenburn
               :website "https://github.com/dbrock/zenburn-el"
               :description "The zenburn color theme for GNU Emacs (for color theme)"
               ;; :depends (color-theme)
               :type github
               :pkgname "dbrock/zenburn-el"
               :prepare (autoload 'color-theme-zenburn "color-theme-zenburn" nil t))
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
        ;; The feature of `smarttabs' is `smart-tabs-mode'.
        ;;
        ;; Install my version because of [[https://github.com/jcsalomon/smarttabs/pull/28]]
        (:name smarttabs
               :pkgname "dochang/smarttabs"
               :branch "keep-indent-setting"
               :library smart-tabs-mode)
        (:name df-mode
               :lazy nil
               :features (df-mode))
        (:name groovy-emacs-mode
               :library groovy-mode)
        ;; Chinese calendar support for anniversaries.
        ;;
        ;; - [[https://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00135.html]]
        ;; - [[http://permalink.gmane.org/gmane.emacs.sources/3252]]
        ;; - [[https://github.com/leoliu/cal-china-plus]]
        ;;
        ;; Has been merged into Emacs 25.1
        (:name cal-china-plus
               :website "https://github.com/leoliu/cal-china-plus"
               :description "extra stuff for cal-china"
               :builtin "25.1"
               :type github
               :pkgname "leoliu/cal-china-plus"
               :library ,(if (version< emacs-version "25.1")
                             'cal-china-plus
                           'cal-china))
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
               :website "http://melpa.milkbox.net/#/erlang"
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
        (:name chinese-fonts-setup
               :depends ())
        ;; `json' has been included with Emacs since February 2008.
        (:name json
               :builtin "24")
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
