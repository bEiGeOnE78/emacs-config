;;; config-treesitter.el --- Tree-sitter configuration for enhanced syntax highlighting

;;; Commentary:
;; This file configures tree-sitter for enhanced syntax highlighting and AST viewing.
;; Tree-sitter provides fast, incremental parsing for better code understanding.

;;; Code:

;;----------------------------------------------------------------------------
;; Package Installation with Error Handling
;;----------------------------------------------------------------------------

;; Ensure package repositories are available
(unless package-archive-contents
  (package-refresh-contents))

;;----------------------------------------------------------------------------
;; Tree-sitter Core
;;----------------------------------------------------------------------------

(use-package tree-sitter
  :ensure t
  :defer t
  :config
  ;; Enable tree-sitter globally for supported modes
  (global-tree-sitter-mode)
  ;; Enable syntax highlighting with tree-sitter
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;----------------------------------------------------------------------------
;; Tree-sitter Language Grammars
;;----------------------------------------------------------------------------

(use-package tree-sitter-langs
  :ensure t
  :defer t
  :after tree-sitter
  :config
  ;; Automatically install and configure language grammars
  ;; This package provides pre-compiled grammars for many languages
  )

;;----------------------------------------------------------------------------
;; Language-specific Tree-sitter Configuration
;;----------------------------------------------------------------------------

;; Ensure tree-sitter-major-mode-language-alist exists
(with-eval-after-load 'tree-sitter
  ;; C/C++
  (add-to-list 'tree-sitter-major-mode-language-alist '(c-mode . c))
  (add-to-list 'tree-sitter-major-mode-language-alist '(c++-mode . cpp))

  ;; Rust
  (add-to-list 'tree-sitter-major-mode-language-alist '(rustic-mode . rust))
  (add-to-list 'tree-sitter-major-mode-language-alist '(rust-mode . rust))

  ;; Lua
  (add-to-list 'tree-sitter-major-mode-language-alist '(lua-mode . lua))

  ;; MATLAB (if supported)
  (add-to-list 'tree-sitter-major-mode-language-alist '(matlab-mode . matlab))

  ;; Additional languages for general development
  (add-to-list 'tree-sitter-major-mode-language-alist '(python-mode . python))
  (add-to-list 'tree-sitter-major-mode-language-alist '(javascript-mode . javascript))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . typescript))
  (add-to-list 'tree-sitter-major-mode-language-alist '(json-mode . json))
  (add-to-list 'tree-sitter-major-mode-language-alist '(yaml-mode . yaml)))

;;----------------------------------------------------------------------------
;; Tree-sitter AST Viewing
;;----------------------------------------------------------------------------

(defun my/show-tree-sitter-ast ()
  "Show the abstract syntax tree for the current buffer using tree-sitter."
  (interactive)
  (if (bound-and-true-p tree-sitter-mode)
      (let ((buf-name "*Tree-sitter AST*"))
        (with-current-buffer (get-buffer-create buf-name)
          (erase-buffer)
          (insert (format "AST for %s:\n\n" (buffer-name (other-buffer))))
          ;; Use the correct tree-sitter API based on what's available
          (cond
           ;; Try newer API first
           ((and (boundp 'tree-sitter-tree) tree-sitter-tree)
            (condition-case nil
                (insert (format "%S" (tree-sitter-node-to-sexp (tree-sitter-root-node tree-sitter-tree))))
              (error (insert "Tree-sitter AST display not supported with current version"))))
           ;; Fallback message
           (t (insert "Tree-sitter is active but AST display not available\nTry: M-x tree-sitter-debug-mode for interactive exploration")))
          (goto-char (point-min))
          (special-mode))
        (display-buffer buf-name))
    (message "Tree-sitter is not enabled in this buffer")))

(defun my/tree-sitter-debug-mode ()
  "Toggle tree-sitter debug mode for AST inspection."
  (interactive)
  (if (bound-and-true-p tree-sitter-debug-mode)
      (progn
        (tree-sitter-debug-mode -1)
        (message "Tree-sitter debug mode disabled"))
    (progn
      (tree-sitter-debug-mode 1)
      (message "Tree-sitter debug mode enabled - hover over syntax nodes for details"))))

(defun my/show-tree-sitter-info ()
  "Show tree-sitter information at point."
  (interactive)
  (if (bound-and-true-p tree-sitter-mode)
      (let* ((node (tree-sitter-node-at-point))
             (type (when node (tree-sitter-node-type node)))
             (start (when node (tree-sitter-node-start node)))
             (end (when node (tree-sitter-node-end node))))
        (if node
            (message "Node: %s, Range: %d-%d" type start end)
          (message "No tree-sitter node at point")))
    (message "Tree-sitter is not enabled in this buffer")))

;;----------------------------------------------------------------------------
;; Enhanced Tree-sitter Features
;;----------------------------------------------------------------------------

(use-package tree-sitter-indent
  :ensure t
  :after tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-indent-mode)
  :config
  ;; Enable intelligent indentation based on AST structure
  )

;;----------------------------------------------------------------------------
;; Tree-sitter Integration with Other Modes
;;----------------------------------------------------------------------------

;; Integration with which-key for tree-sitter commands
(with-eval-after-load 'which-key
  (add-to-list 'which-key-replacement-alist
               '(("C-c t" . "Prefix Command") . ("C-c t" . "tree-sitter"))))

;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------

;; Tree-sitter specific key bindings
(define-prefix-command 'my/tree-sitter-map)
(global-set-key (kbd "C-c t") 'my/tree-sitter-map)

(define-key my/tree-sitter-map (kbd "a") 'my/show-tree-sitter-ast)
(define-key my/tree-sitter-map (kbd "i") 'my/show-tree-sitter-info)
(define-key my/tree-sitter-map (kbd "d") 'my/tree-sitter-debug-mode)
(define-key my/tree-sitter-map (kbd "r") 'tree-sitter-debug-reparse)
(define-key my/tree-sitter-map (kbd "h") 'tree-sitter-hl-mode)

;;----------------------------------------------------------------------------
;; Hooks for automatic tree-sitter activation
;;----------------------------------------------------------------------------

;; Enable tree-sitter for programming modes
(add-hook 'prog-mode-hook 'tree-sitter-mode)
(add-hook 'prog-mode-hook 'tree-sitter-hl-mode)

;;----------------------------------------------------------------------------
;; Performance optimizations
;;----------------------------------------------------------------------------

(with-eval-after-load 'tree-sitter
  ;; Optimize tree-sitter performance
  (setq tree-sitter-debug-jump-buttons nil)
  (setq tree-sitter-debug-highlight-jump-region nil))

;;----------------------------------------------------------------------------
;; Export module
;;----------------------------------------------------------------------------
(provide 'config-treesitter)

;;; config-treesitter.el ends here