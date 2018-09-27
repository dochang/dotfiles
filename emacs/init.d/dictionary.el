;;; Dictionary Mode
;; [[http://www.myrkr.in-berlin.de/dictionary/]]
;; [[https://github.com/myrkr/dictionary-el]]

(defun $dictionary-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil))

(req-package dictionary
  :bind (("C-c d" . $hydra-dictionary/body)
         :map dictionary-mode-map
         ("DEL" . scroll-down))
  :hook (dictionary-mode . $dictionary-mode-hook)
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-tooltip-dictionary "!")
  :init
  (defhydra $hydra-dictionary (:color teal)
    "dictionary"
    ("s" dictionary-search "search")
    ("m" dictionary-match-words "match words")
    ("d" dictionary-lookup-definition "lookup definition")
    ("q" nil "quit"))
  :config
  (global-dictionary-tooltip-mode 1))
