;;; config-treesitter-fallback.el --- Fallback tree-sitter configuration

;;; Commentary:
;; This file provides a fallback when tree-sitter packages are unavailable.
;; It enhances built-in syntax highlighting and provides basic AST-like features.

;;; Code:

;;----------------------------------------------------------------------------
;; Enhanced Built-in Syntax Highlighting
;;----------------------------------------------------------------------------

;; Enable font-lock globally
(global-font-lock-mode 1)

;; Maximum font-lock decoration
(setq font-lock-maximum-decoration t)

;; Enable syntax highlighting for more modes
(add-hook 'prog-mode-hook 'font-lock-mode)

;;----------------------------------------------------------------------------
;; Basic AST-like Features
;;----------------------------------------------------------------------------

(defun my/show-syntax-info ()
  "Show basic syntax information at point."
  (interactive)
  (let ((face (get-text-property (point) 'face))
        (syntax (char-syntax (char-after))))
    (message "Face: %s, Syntax: %c" face syntax)))

(defun my/basic-code-outline ()
  "Show a basic code outline using imenu."
  (interactive)
  (if (featurep 'imenu)
      (call-interactively 'imenu)
    (message "Imenu not available")))

;;----------------------------------------------------------------------------
;; Key Bindings - Basic Tree-sitter-like Functions  
;;----------------------------------------------------------------------------

(define-prefix-command 'my/syntax-map)
(global-set-key (kbd "C-c t") 'my/syntax-map)

(define-key my/syntax-map (kbd "s") 'my/show-syntax-info)
(define-key my/syntax-map (kbd "o") 'my/basic-code-outline)
(define-key my/syntax-map (kbd "h") 'font-lock-fontify-buffer)

;; Integration with which-key
(with-eval-after-load 'which-key
  (add-to-list 'which-key-replacement-alist
               '(("C-c t" . "Prefix Command") . ("C-c t" . "syntax"))))

;;----------------------------------------------------------------------------
;; Language-specific enhancements
;;----------------------------------------------------------------------------

;; Enhanced C/C++ highlighting
(add-hook 'c-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

(add-hook 'c++-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

;;----------------------------------------------------------------------------
;; Message about tree-sitter availability
;;----------------------------------------------------------------------------

(message "Tree-sitter not available - using enhanced built-in syntax highlighting")

;;----------------------------------------------------------------------------
;; Export module
;;----------------------------------------------------------------------------
(provide 'config-treesitter-fallback)

;;; config-treesitter-fallback.el ends here