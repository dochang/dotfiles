;; Ruby Block Mode
;;
;; [[http://www.emacswiki.org/emacs/RubyBlockMode]]

(req-package ruby-block
  :ensure nil
  :quelpa (ruby-block :fetcher github :repo "emacsmirror/ruby-block")
  :init
  (autoload 'ruby-block-mode "ruby-block" nil t)
  (setq ruby-block-highlight-toggle t))
