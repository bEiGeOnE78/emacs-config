;;; config-lsp-fallback.el --- Fallback LSP configuration when packages are unavailable

;;; Commentary:
;; This file provides a fallback configuration when LSP packages are unavailable.
;; It sets up basic language modes without advanced LSP features.

;;; Code:

;;----------------------------------------------------------------------------
;; Basic Language Mode Support
;;----------------------------------------------------------------------------

;; C/C++ basic support
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

;; Basic C/C++ configuration
(add-hook 'c-mode-hook 
          (lambda () 
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)))

(add-hook 'c++-mode-hook 
          (lambda () 
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)))

;; Rust basic support (built-in rust-mode if available)
(when (require 'rust-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; Lua support - try to install lua-mode if available
(condition-case nil
    (use-package lua-mode
      :ensure t
      :mode "\\.lua\\'")
  (error 
   (message "lua-mode not available, using fundamental mode for .lua files")
   (add-to-list 'auto-mode-alist '("\\.lua\\'" . fundamental-mode))))

;; MATLAB support - try to install matlab-mode if available  
(condition-case nil
    (use-package matlab-mode
      :ensure t
      :mode "\\.m\\'")
  (error
   (message "matlab-mode not available, using octave-mode for .m files")
   (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))))

;;----------------------------------------------------------------------------
;; Basic IDE Features without LSP
;;----------------------------------------------------------------------------

;; Enable imenu for function navigation
(add-hook 'prog-mode-hook 'imenu-add-menubar-index)

;; Enable electric pair mode for bracket completion
(electric-pair-mode 1)

;; Enable show-paren-mode for bracket highlighting
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Enable highlight-symbol for basic symbol highlighting
(condition-case nil
    (use-package highlight-symbol
      :ensure t
      :hook (prog-mode . highlight-symbol-mode)
      :config
      (setq highlight-symbol-idle-delay 1.0))
  (error (message "highlight-symbol not available")))

;;----------------------------------------------------------------------------
;; Key Bindings - Basic Navigation
;;----------------------------------------------------------------------------

;; Basic navigation bindings that work without LSP
(define-prefix-command 'my/basic-nav-map)
(global-set-key (kbd "C-c l") 'my/basic-nav-map)

(define-key my/basic-nav-map (kbd "i") 'imenu)
(define-key my/basic-nav-map (kbd "f") 'find-file-at-point)
(define-key my/basic-nav-map (kbd "g") 'grep-find)
(define-key my/basic-nav-map (kbd "o") 'occur)

;; Integration with which-key
(with-eval-after-load 'which-key
  (add-to-list 'which-key-replacement-alist
               '(("C-c l" . "Prefix Command") . ("C-c l" . "basic-nav"))))

;;----------------------------------------------------------------------------
;; Message about LSP availability
;;----------------------------------------------------------------------------

(message "LSP-mode not available - using basic language support. Install clangd, rust-analyzer, etc. manually if needed.")

;;----------------------------------------------------------------------------
;; Export module
;;----------------------------------------------------------------------------
(provide 'config-lsp-fallback)

;;; config-lsp-fallback.el ends here