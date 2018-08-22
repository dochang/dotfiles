(req-package cflow
  :ensure nil
  :quelpa (cflow
           :fetcher git
           :url "https://git.savannah.gnu.org/git/cflow.git"
           :files ("elisp/cflow-mode.el"))
  :mode ("\\.cflow$" . cflow-mode))
