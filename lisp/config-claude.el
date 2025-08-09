;;; config-claude.el --- Claude AI integration configuration

;;; Commentary:
;; This file contains Claude AI integration configurations for Emacs,
;; implementing a layered approach with gptel for chat and claude-code-ide
;; for terminal integration.

;;; Code:

;;----------------------------------------------------------------------------
;; Phase 1: gptel for Claude Chat Integration
;;----------------------------------------------------------------------------

(use-package gptel
  :ensure t
  :config
  ;; First, set up API key from environment variable, credentials file, or auth-source
  ;; Try multiple sources for the API key
  (setq gptel-api-key 
        (or (getenv "ANTHROPIC_API_KEY")
            ;; Try to read directly from Claude credentials file
            (when (and (file-exists-p "~/.claude/.credentials.json")
                       (executable-find "jq"))
              (condition-case nil
                (with-temp-buffer
                  (call-process "jq" nil t nil "-r" ".claudeAiOauth.accessToken" 
                                (expand-file-name "~/.claude/.credentials.json"))
                  (string-trim (buffer-string)))
                (error nil)))
            (auth-source-pick-first-password :host "api.anthropic.com")))
  
  ;; Configure Claude backend (after API key is set)
  (setq gptel-model 'claude-3-5-sonnet-20241022)
  (setq gptel-backend 
        (gptel-make-anthropic "Claude"
          :stream t
          :key gptel-api-key))
  
  ;; Configure gptel defaults
  (setq gptel-default-mode 'org-mode)
  
  ;; Custom system prompts for coding assistance
  (setq gptel-directives
        '((default . "You are a helpful coding assistant.")
          (code-review . "You are an expert code reviewer. Focus on code quality, best practices, and potential issues.")
          (debugging . "You are a debugging expert. Help identify and fix code issues.")
          (documentation . "You are a technical writer. Help create clear, comprehensive documentation."))))

;;----------------------------------------------------------------------------
;; Key bindings for Phase 1
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-c a c") 'gptel)  ; Start Claude chat
(global-set-key (kbd "C-c a s") 'gptel-send)  ; Send current region/buffer to Claude
(global-set-key (kbd "C-c a r") 'gptel-set-directive)  ; Set system prompt
(global-set-key (kbd "C-c a k") 'claude-reload-api-key)  ; Reload API key
(global-set-key (kbd "C-c a d") 'claude-debug-config)  ; Debug configuration

;;----------------------------------------------------------------------------
;; Helper functions for Phase 1
;;----------------------------------------------------------------------------

(defun claude-reload-api-key ()
  "Reload the Claude API key from available sources and recreate backend."
  (interactive)
  (setq gptel-api-key 
        (or (getenv "ANTHROPIC_API_KEY")
            ;; Try to read directly from Claude credentials file
            (when (and (file-exists-p "~/.claude/.credentials.json")
                       (executable-find "jq"))
              (condition-case nil
                (with-temp-buffer
                  (call-process "jq" nil t nil "-r" ".claudeAiOauth.accessToken" 
                                (expand-file-name "~/.claude/.credentials.json"))
                  (string-trim (buffer-string)))
                (error nil)))
            (auth-source-pick-first-password :host "api.anthropic.com")))
  
  ;; Recreate the backend with the new API key
  (when gptel-api-key
    (setq gptel-backend 
          (gptel-make-anthropic "Claude"
            :stream t
            :key gptel-api-key)))
  
  (message "API key reloaded: %s" 
           (if gptel-api-key 
               (format "%d characters" (length gptel-api-key))
               "NOT FOUND")))

(defun claude-debug-config ()
  "Show current Claude configuration status."
  (interactive)
  (message "=== Claude Configuration Debug ===")
  (message "Environment API key: %s" 
           (if (getenv "ANTHROPIC_API_KEY") 
               (format "%d chars" (length (getenv "ANTHROPIC_API_KEY")))
               "NOT SET"))
  (message "gptel-api-key: %s" 
           (if gptel-api-key 
               (format "%d chars" (length gptel-api-key))
               "NOT SET"))
  (message "Backend configured: %s" (if gptel-backend "YES" "NO"))
  (message "Model: %s" gptel-model))

(defun claude-chat-code-review ()
  "Start a Claude chat session with code review directive."
  (interactive)
  (gptel-set-directive 'code-review)
  (gptel))

(defun claude-chat-debugging ()
  "Start a Claude chat session with debugging directive."
  (interactive)
  (gptel-set-directive 'debugging)
  (gptel))

(defun claude-send-buffer-for-review ()
  "Send current buffer to Claude for code review."
  (interactive)
  (gptel-set-directive 'code-review)
  (gptel-send))

;;----------------------------------------------------------------------------
;; Export module
;;----------------------------------------------------------------------------
(provide 'config-claude)

;;; config-claude.el ends here