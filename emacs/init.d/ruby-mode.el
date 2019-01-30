;;; Ruby Mode

(req-package ruby-mode
  :mode (;; For Gem
         ("\\.gemspec\\'" . ruby-mode)
         ;; For Bundler
         ("Gemfile\\'" . ruby-mode)
         ;; For Rack
         ("config\\.ru\\'" . ruby-mode)
         ;; For Rake
         ("\\.rake\\'" . ruby-mode)
         ;; For Homebrew
         ("Brewfile\\'" . ruby-mode)
         ))
