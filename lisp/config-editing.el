;; Add new lines at bottom of screen with "C-n"
(setq next-line-add-newlines t)

;;; --- Modern Commenting Style ---
;; Use the 'indent' style, which creates ';;' at the start of lines.
(setq-default comment-style 'indent)

;; Stop forcing single-line comments to the end of the line.
(setq-default comment-column 0)

;; Sentences end with single space
(setq sentence-end-double-space nil)

;;------------------------------------------------------------------------------
;; Completion Engine
;;------------------------------------------------------------------------------
(fido-vertical-mode 1)
(setq completion-styles '(basic flex)
      completion-auto-select t
      completion-auto-help 'visible
      completion-ignore-case t
      completion-preview-mode t
      completions-format 'one-column
      completions-sort 'historical
      completions-max-height 20)

;;------------------------------------------------------------------------------
;; Export module
;;------------------------------------------------------------------------------
(provide 'config-editing)
