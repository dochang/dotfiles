;;; Dictionary Mode
;; [[http://www.myrkr.in-berlin.de/dictionary/]]
;; [[https://github.com/myrkr/dictionary-el]]

(defun $dictionary-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil))

(req-package dictionary
  :require hydra
  :bind (("C-c d" . $hydra-dictionary/body)
         :map dictionary-mode-map
         ("DEL" . scroll-down))
  :hook (dictionary-mode . $dictionary-mode-hook)
  :init
  (setq dictionary-use-single-buffer t)
  (setq dictionary-tooltip-dictionary "!")
  (defhydra $hydra-dictionary (:color teal)
    "dictionary"
    ("s" dictionary-search "search")
    ("m" dictionary-match-words "match words")
    ("d" dictionary-lookup-definition "lookup definition")
    ("q" nil "quit"))
  :config
  (global-dictionary-tooltip-mode 1))
