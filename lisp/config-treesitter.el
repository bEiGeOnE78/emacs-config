;;; config-treesitter.el --- Tree-sitter configuration for enhanced syntax highlighting

;;; Commentary:
;; This file configures tree-sitter for enhanced syntax highlighting and AST viewing.
;; Tree-sitter provides fast, incremental parsing for better code understanding.

;;; Code:

;;----------------------------------------------------------------------------
;; Tree-sitter Core
;;----------------------------------------------------------------------------
(add-to-list 'display-buffer-alist
	     '("^*tree-sitter explorer *" display-buffer-in-side-window
	       (side . right)
	       (window-width . 0.40)))

;;----------------------------------------------------------------------------
;; Tree-sitter Language Grammars
;;----------------------------------------------------------------------------

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(c "http://github.com/tree-sitter/tree-sitter-c")
	(cpp "http://github.com/tree-sitter/tree-sitter-cpp")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
	(lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(matlab "https://github.com/acristoffers/tree-sitter-matlab")
        (python "https://github.com/tree-sitter/tree-sitter-python")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(dolist (lang (mapcar #'car treesit-language-source-alist))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (cpp-mode . cpp-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (html-mode . html-ts-mode)
        (javascript-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (lua-mode . lua-ts-mode)
        (python-mode . python-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;;----------------------------------------------------------------------------
;; Plugins
;;----------------------------------------------------------------------------
(use-package treesit-fold
  :load-path "/home/vader/.emacs.d/elpa/treesit-fold-0.2.1")

(add-hook 'emacs-lisp-mode-hook (lambda () (treesit-parser-create 'elisp)))

(use-package treesit-fold-indicators
  :load-path "/home/vader/.emacs.d/elpa/treesit-fold-0.2.1")

(with-eval-after-load 'treesit-fold
  (require 'treesit-fold-level)
  (treesit-fold-level-setup-keys))

;;----------------------------------------------------------------------------
;; Export module
;;----------------------------------------------------------------------------
(provide 'config-treesitter)

;;; config-treesitter.el ends here
