(add-to-list 'load-path "~/.emacs.d/els/org-init/")
(require 'org-init)
(org-init-compile)
(load-file org-init--elc-file)

