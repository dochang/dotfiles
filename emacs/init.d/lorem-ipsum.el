(req-package lorem-ipsum

  :bind (("C-c l" . $hydra-lorem-ipsum/body))

  :init

  (defhydra $hydra-lorem-ipsum (:color teal)
    "lorem ipsum"
    ("s" lorem-ipsum-insert-sentences "insert sentences")
    ("p" lorem-ipsum-insert-paragraphs "insert paragraphs")
    ("l" lorem-ipsum-insert-list "insert list")))
