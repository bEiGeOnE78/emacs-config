(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'config-packages)
(require 'config-editing)
(require 'config-bindings)
(require 'config-orgmode)
(require 'config-ui)
(require 'config-version-control)

;; Try to load LSP configuration, fallback to basic if packages unavailable
(condition-case err
    (require 'config-lsp)
  (error 
   (message "LSP packages unavailable, using fallback configuration: %s" err)
   (require 'config-lsp-fallback)))

;; Try to load tree-sitter configuration, fallback to basic if packages unavailable  
(condition-case err
    (require 'config-treesitter)
  (error
   (message "Tree-sitter packages unavailable, using fallback configuration: %s" err)
   (require 'config-treesitter-fallback)))

;; (require 'config-proxy)

;; Try to load Claude configuration, fallback gracefully if packages unavailable
(condition-case err
    (require 'config-claude)
  (error 
   (message "Claude packages unavailable, skipping Claude configuration: %s" err)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1f4ecd53b893aa4462c906ea10ac662b5d90ef0f087bb00e217fbc91b4d67d11"
     "9c6aa7eb1bde73ba1142041e628827492bd05678df4d9097cda21b1ebcb8f8b9"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
