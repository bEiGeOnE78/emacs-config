(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'config-packages)
(require 'config-editing)
(require 'config-bindings)
(require 'config-orgmode)
(require 'config-ui)
(require 'config-version-control)
(require 'config-lsp)
(require 'config-treesitter)
;; (require 'config-proxy)














