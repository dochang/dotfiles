;; https://github.com/tarsius/keycast/blob/46370b8a72922902921d3ed2fa194564568053dc/keycast.el#L8
(unless (version< emacs-version "25.3")
  (req-package keycast
    :hook (emacs-startup . keycast-mode)
    :custom
    (keycast-remove-tail-elements nil)))
