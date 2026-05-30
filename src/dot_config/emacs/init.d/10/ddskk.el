;; -*- lexical-binding: t; -*-

;; Put all configurations into `${XDG_CONFIG_HOME:-$HOME/.config}/ddskk'.

(setup (:package ddskk)

  (require 'xdg)
  (setopt skk-user-directory
          (expand-file-name "ddskk" (xdg-config-home)))

  (setopt skk-byte-compile-init-file nil)

  )
