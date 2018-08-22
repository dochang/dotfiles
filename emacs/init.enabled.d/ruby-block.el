;; Ruby Block Mode
;;
;; [[http://www.emacswiki.org/emacs/RubyBlockMode]]

(req-package ruby-block
  :ensure nil
  :quelpa (ruby-block :fetcher github :repo "emacsmirror/ruby-block")
  :commands (ruby-block-mode)
  :init
  (setq ruby-block-highlight-toggle t))
