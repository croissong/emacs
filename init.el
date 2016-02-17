(add-to-list 'load-path "~/.emacs.d/els/org-init/")
(require 'org-init)
(org-init--tangle)
(load-file org-init--temp-file)
