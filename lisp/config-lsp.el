;;; config-lsp.el --- Language Server Protocol configuration

;;; Commentary:
;; This file configures LSP-mode and language servers for C/C++, Rust, MATLAB, and Lua.
;; Provides comprehensive IDE-like features including completion, diagnostics, and navigation.

;;; Code:

;;----------------------------------------------------------------------------
;; Package Installation with Error Handling
;;----------------------------------------------------------------------------

;; Ensure package repositories are available
(unless package-archive-contents
  (package-refresh-contents))

;;----------------------------------------------------------------------------
;; LSP Mode - Core Language Server Support
;;----------------------------------------------------------------------------

(use-package lsp-mode
  :ensure t
  :defer t
  :init
  ;; Set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook 
  ;; Enable LSP for specific modes
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (lua-mode . lsp-deferred)
   (matlab-mode . lsp-deferred)
   ;; Enable which-key integration for LSP commands
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  ;; Performance optimizations
  (setq lsp-completion-provider :none) ;; Use corfu/company instead
  (setq lsp-idle-delay 0.1)
  (setq lsp-log-io nil) ;; Disable logging for better performance
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  ;; File watching settings
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-auto-guess-root nil))

;;----------------------------------------------------------------------------
;; LSP UI - Enhanced User Interface
;;----------------------------------------------------------------------------

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  ;; Configure LSP UI components
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-enable nil) ;; Disable sideline
  (setq lsp-ui-doc-enable nil) ;; Disable hover documentation popup
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-peek-height 20)
  (setq lsp-ui-peek-list-width 50)
  (setq lsp-ui-peek-fontify 'on-demand))

;;----------------------------------------------------------------------------
;; Language Servers Configuration
;;----------------------------------------------------------------------------

;; C/C++ - clangd
(use-package lsp-mode
  :config
  (setq lsp-clients-clangd-args 
        '("--header-insertion=never"
          "--clang-tidy"
          "--completion-style=detailed"
          "--function-arg-placeholders"
          "--fallback-style=llvm"))
  (setq lsp-clangd-binary-path "clangd"))

;; Rust - rust-analyzer
(use-package rustic
  :ensure t
  :defer t
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (when (featurep 'lsp-mode)
    (setq rustic-lsp-client 'lsp-mode))
  (setq rustic-format-on-save nil)) ;; Disable auto-format on save

;; Lua
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  ;; Configure lua-language-server if available
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(lua-mode . "lua"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "lua-language-server")
                      :major-modes '(lua-mode)
                      :server-id 'lua-language-server))))

;; MATLAB
(use-package matlab
  :ensure matlab-mode
  :mode "\\.m\\'"
  :config
  ;; Basic MATLAB mode configuration
  (setq matlab-indent-function-body t)
  (setq matlab-verify-on-save-flag nil)
  ;; Configure MATLAB Language Server if available
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(matlab-mode . "matlab"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "matlab-language-server")
                      :major-modes '(matlab-mode)
                      :server-id 'matlab-language-server))))

;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------

;; Global LSP key bindings
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l a") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c l d") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c l i") 'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "C-c l t") 'lsp-find-type-definition)
  (define-key lsp-mode-map (kbd "C-c l s") 'lsp-ivy-workspace-symbol))

;;----------------------------------------------------------------------------
;; Export module
;;----------------------------------------------------------------------------
(provide 'config-lsp)

;;; config-lsp.el ends here