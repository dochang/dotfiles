#!/bin/sh

":";exec emacs -Q --script "$0"

;; Be more verbose.
(setq debug-on-error t)

(setq make-backup-files nil)

(load (locate-user-emacs-file "packages") t)

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

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
      (let (el-get-master-branch)
        (goto-char (point-max))
        (eval-print-last-sexp)))))

(unless (or (and (require 'package nil 'noerror)
                 (progn
                   (package-initialize)
                   (require 'mb-url nil 'noerror)))
            (el-get 'sync mb-url))
  (el-get-bundle! mb-url in dochang/mb-url
    :branch "master"
    :depends ()
    :library mb-url))

(advice-add 'url-http :override 'mb-url-http-curl)

(unless (require 'package nil 'noerror)
  (el-get 'sync 'package))

;; TODO: This needs to be removed in the future!
;;
;; http://stackoverflow.com/a/26110978
;; https://github.com/bbatsov/prelude/issues/699
(setq package-check-signature nil)

(package-initialize)

(package-refresh-contents)

(mapc 'package-install
      (append '(emms
                go-mode
                undo-tree
                slime
                magit
                paredit
                fill-column-indicator
                rainbow-delimiters
                inf-ruby
                ruby-block
                haml-mode
                sass-mode
                scss-mode
                less-css-mode
                markdown-mode
                lua-mode
                coffee-mode
                slim-mode
                jade-mode
                stylus-mode
                yaml-mode
                mustache-mode
                flycheck
                pandoc-mode
                editorconfig
                clojure-mode
                cider
                flymake-easy
                flymake-ruby
                flymake-coffee
                flymake-shell
                flymake-css
                flymake-jshint
                flymake-jslint
                offlineimap
                ox-impress-js
                sublimity
                mmm-mode
                rainbow-mode
                json-mode
                fvwm-mode
                textile-mode
                yari
                uuidgen
                color-theme
                tangotango-theme
                zenburn-theme
                org-plus-contrib
                f
                livescript-mode
                handlebars-mode
                groovy-mode
                dictionary
                multiple-cursors
                erlang
                elixir-mode
                php-mode
                rust-mode
                conkeror-minor-mode
                dockerfile-mode
                elm-mode
                jinja2-mode
                restclient
                pip-requirements
                httprepl
                toml-mode
                haskell-mode
                cmake-mode
                puppet-mode
                wsd-mode
                form-feed
                gitconfig-mode
                gitignore-mode
                gitattributes-mode
                chinese-fonts-setup
                password-store
                flycheck-package
                emms-player-mpv
                todotxt
                android-mode
                lorem-ipsum
                fringe-current-line
                indent-guide
                cedit
                download-region
                docean
                http
                vagrant
                which-key
                nginx-mode
                jsx-mode
                symon
                flycheck-pos-tip
                flymake-cursor
                docker
                docker-tramp
                marcopolo
                pass
                auth-password-store
                focus-autosave-mode
                nix-mode
                emmet-mode
                dired+
                systemd
                el-pocket
                what-the-commit
                web-mode
                smart-mark
                git-annex
                magit-annex
                writeroom-mode
                transmission
                auto-package-update
                describe-number
                cargo
                color-theme-modern
                editorconfig-core
                ruby-additional
                pointback
                olivetti
                flymake-less
                diredful
                el2markdown
                mb-url)
              (if (< emacs-major-version 24)
                  '(scala-mode)
                '(scala-mode2
                  sbt-mode))))

(el-get 'sync
        (append '(el-get
                  vcard-mode
                  lua2-mode
                  lua-block
                  yaml-path
                  ri
                  cflow
                  color-theme-blackboard
                  color-theme-hober2
                  hober2-theme
                  color-theme-tango
                  color-theme-tangotango
                  color-theme-empty-void
                  color-theme-inkpot
                  color-theme-wombat
                  color-theme-wombat-dark
                  zenburn
                  smarttabs
                  df-mode)
                ;; cal-china-plus has been merged into Emacs 25.1
                (if (version< emacs-version "25.1")
                    '(cal-china-plus)
                  '())))

;; Delete el-get bootstrap mb-url
(el-get-remove 'mb-url)

;; Local Variables:
;; mode: emacs-lisp
;; End:
