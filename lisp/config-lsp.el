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
  :config
  (setq lsp-enable-folding t)
  :hook
  ;; Enable LSP for specific modes
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (lua-mode . lsp-deferred)
   (matlab-mode . lsp-deferred)
   ;; Enable which-key integration for LSP commands
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(setq hs-set-up-overlay t)
  ;; :config
  ;; Performance optimizations
  ;; (setq lsp-completion-provider :none) ;; Use corfu/company instead
  ;; (setq lsp-idle-delay 0.1)
  ;; (setq lsp-log-io nil) ;; Disable logging for better performance
  ;; (setq lsp-keep-workspace-alive nil)
  ;; (setq lsp-enable-symbol-highlighting t)
  ;; (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-modeline-code-actions-enable nil)
  ;; (setq lsp-modeline-diagnostics-enable nil)
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-semantic-tokens-enable nil)
  ;; (setq lsp-enable-folding nil)
  ;; (setq lsp-enable-imenu nil)
  ;; (setq lsp-enable-snippet nil)
  ;; ;; File watching settings
  ;; (setq lsp-file-watch-threshold 2000)
  ;; (setq lsp-auto-guess-root nil))

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
(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
  
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

;; Python
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook (python-ts-mode . (lambda ()
			    (require 'lsp-mode)
			    (lsp-deferred))))

(use-package flymake-ruff
  :ensure t
  :hook (python-ts-mode . flymake-ruff-load))

;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
	 (before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports))
  :config
  (setq gofmt-command "goimports"))

;; DAP Mode
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode python-ts-mode) . dap-mode)
  :config
  ;; Enable dap-ui for a better debugging experience
  (dap-ui-mode 1)
  ;; Enable tooltip on hover
  (dap-tooltip-mode 1)
  ;; Use tooltips for mouse hover
  (tooltip-mode 1)
  ;; Enable dap-ui controls
  (dap-ui-controls-mode 1)
  :bind
  (:map dap-mode-map
	("<f5>" . dap-debug)
	("<f9>" . dap-breakpoint-toggle)
	("<f10>" . dap-next)
	("<f11>" . dap-step-in)
	("<f12>" . dap-step-out)))

(use-package dap-python
  :ensure nil
  :after dap-mode
  :config
  ;; Set the Python interpreter and debugpy path
  (setq dap-python-debugger 'debugpy
	dap-python-executable "~/.local/share/BVTool/Photo Tool/photo-env/bin/python"))
  ;; (require 'dap-go)
  ;; (dap-go-setup))

;; gopls specific settings
(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)
   ("gopls.usePlaceholders" t t)
   ("gopls.analyses.unusedparams" t t)
   ("gopls.analyses.shadow" t t)
   ("gopls.analyses.fieldalignment" t t)))

;; Performance tuning
(setq lsp-gopls-server-path "gopls"
      lsp-gopls-server-args '("-remote=auto")
      lsp-gopls-complete-unimported t
      lsp-gopls-use-placeholders t
      lsp-gopls-staticcheck t)

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
