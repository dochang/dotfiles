;; Ruby Block Mode
;;
;; [[http://www.emacswiki.org/emacs/RubyBlockMode]]

(req-package ruby-block
  :ensure (ruby-block :pin :quelpa)
  :quelpa (ruby-block :fetcher github :repo "emacsmirror/ruby-block")
  :commands (ruby-block-mode)
  :hook (ruby-mode . ruby-block-mode)
  :custom
  (ruby-block-highlight-toggle t))
