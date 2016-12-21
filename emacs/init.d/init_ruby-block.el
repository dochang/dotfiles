;; Ruby Block Mode
;;
;; [[http://www.emacswiki.org/emacs/RubyBlockMode]]

(req-package ruby-block
  :init
  (autoload 'ruby-block-mode "ruby-block" nil t)
  (setq ruby-block-highlight-toggle t))
