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
(global-set-key (kbd "C-c a u") 'claude-unified-chat)     ; Unified Claude experience

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
;; Phase 2: claude-code-ide.el for Terminal Integration with MCP
;;----------------------------------------------------------------------------

;; Ensure Claude CLI is in exec-path
(let ((node-bin-path (expand-file-name "~/.nvm/versions/node/v24.5.0/bin")))
  (when (file-directory-p node-bin-path)
    (add-to-list 'exec-path node-bin-path)
    (setenv "PATH" (concat node-bin-path ":" (getenv "PATH")))))

;; Ensure project.el is available for project detection
(require 'project)

;; Install eat terminal emulator (pure Elisp, no system dependencies)
(use-package eat
  :ensure t
  :config
  ;; Configure eat for better integration
  (setq eat-term-name "xterm-256color")
  (setq eat-kill-buffer-on-exit t))

;; Manual installation approach for claude-code-ide (not in MELPA)
;; We'll implement core functionality directly since the package isn't easily installable
;; This provides similar terminal integration functionality

(defvar claude-terminal-buffer nil
  "Current Claude terminal buffer.")

(defvar claude-terminal-process nil
  "Current Claude terminal process.")

(defun claude-code-terminal ()
  "Start Claude Code in an eat terminal buffer."
  (interactive)
  (require 'eat)
  (let* ((project (project-current))
         (project-root (if project (project-root project) default-directory))
         (buffer-name (format "*Claude Code - %s*" 
                             (file-name-nondirectory 
                              (directory-file-name project-root)))))
    
    ;; Create or switch to Claude terminal buffer
    (if (and (get-buffer buffer-name)
             (buffer-live-p (get-buffer buffer-name)))
        (switch-to-buffer-other-window buffer-name)
      (progn
        ;; Start eat terminal with Claude Code
        (let ((default-directory project-root))
          (setq claude-terminal-buffer 
                (eat-make buffer-name "claude"))
          (with-current-buffer claude-terminal-buffer
            (setq-local claude-project-root project-root))
          (display-buffer claude-terminal-buffer 
                         '((display-buffer-in-side-window)
                           (side . right)
                           (window-width . 0.4))))))))

(defun claude-code-project ()
  "Start Claude Code with explicit project context."
  (interactive)
  (claude-code-terminal))

(defun claude-code-init ()
  "Initialize Claude Code in current project."
  (interactive)
  (claude-code-with-command "init"))

(defun claude-code-resume ()
  "Resume previous Claude Code session."
  (interactive)
  (claude-code-with-command "resume"))

(defun claude-code-with-command (command)
  "Start Claude Code with a specific command."
  (require 'eat)
  (let* ((project (project-current))
         (project-root (if project (project-root project) default-directory))
         (buffer-name (format "*Claude %s - %s*" 
                             command
                             (file-name-nondirectory 
                              (directory-file-name project-root)))))
    
    ;; Create or switch to Claude terminal buffer
    (if (and (get-buffer buffer-name)
             (buffer-live-p (get-buffer buffer-name)))
        (switch-to-buffer-other-window buffer-name)
      (progn
        ;; Start eat terminal with Claude Code
        (let ((default-directory project-root))
          (setq claude-terminal-buffer 
                (eat-make buffer-name "claude" command))
          (with-current-buffer claude-terminal-buffer
            (setq-local claude-project-root project-root)
            (setq-local claude-command command))
          (display-buffer claude-terminal-buffer 
                         '((display-buffer-in-side-window)
                           (side . right)
                           (window-width . 0.4))))))))

(defun claude-code-menu ()
  "Simple menu for Claude Code options."
  (interactive)
  (let ((choice (read-char-choice 
                 "Claude: (t)erminal, (i)nit, (r)esume, (p)roject, (s)tatus: "
                 '(?t ?i ?r ?p ?s))))
    (pcase choice
      (?t (claude-code-terminal))
      (?i (claude-code-init))
      (?r (claude-code-resume))
      (?p (claude-code-project))
      (?s (claude-project-status)))))

(defun claude-send-file-to-terminal ()
  "Send current file content to Claude terminal."
  (interactive)
  (when (and claude-terminal-buffer
             (buffer-live-p claude-terminal-buffer))
    (let ((file-content (format "Here's the current file %s:\n\n```%s\n%s\n```\n"
                               (buffer-file-name)
                               (file-name-extension (buffer-file-name) t)
                               (buffer-string))))
      (with-current-buffer claude-terminal-buffer
        (eat-term-send-string eat-terminal file-content)))))

(defun claude-execute-in-context ()
  "Execute a command in Claude with current project context."
  (interactive)
  (when (and claude-terminal-buffer
             (buffer-live-p claude-terminal-buffer))
    (let ((command (read-string "Claude command: ")))
      (with-current-buffer claude-terminal-buffer
        (eat-term-send-string eat-terminal (concat command "\n"))))))

;;----------------------------------------------------------------------------
;; Key bindings for Phase 2
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-c a i") 'claude-code-menu)           ; Interactive menu
(global-set-key (kbd "C-c a t") 'claude-code-terminal)      ; Start Claude Code terminal
(global-set-key (kbd "C-c a p") 'claude-code-project)       ; Start with project context
(global-set-key (kbd "C-c a n") 'claude-code-init)          ; Initialize new Claude session
(global-set-key (kbd "C-c a R") 'claude-code-resume)        ; Resume previous session
(global-set-key (kbd "C-c a f") 'claude-send-file-to-terminal) ; Send current file
(global-set-key (kbd "C-c a x") 'claude-execute-in-context) ; Execute in project context
(global-set-key (kbd "C-c a g") 'claude-send-region-to-terminal) ; Send region to Claude terminal

;;----------------------------------------------------------------------------
;; Helper functions for Phase 2
;;----------------------------------------------------------------------------

(defun claude-project-status ()
  "Show current project context and Claude Code status."
  (interactive)
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (claude-buffer (get-buffer-create "*Claude Project Status*")))
    (with-current-buffer claude-buffer
      (erase-buffer)
      (insert "=== Claude Code Project Integration Status ===\n\n")
      (insert (format "Current Project: %s\n" 
                      (or project-root "No project detected")))
      (insert (format "Claude CLI Available: %s\n" 
                      (if (executable-find "claude") 
                          (format "YES (%s)" (executable-find "claude"))
                          "NO")))
      (insert (format "eat Backend: %s\n" 
                      (if (featurep 'eat) "Loaded" "Not loaded")))
      (insert (format "claude-code-ide: %s\n" 
                      (if (featurep 'claude-code-ide) "Loaded" "Not loaded")))
      (insert "\nActive Claude Buffers:\n")
      (dolist (buffer (buffer-list))
        (when (string-match-p "Claude Code" (buffer-name buffer))
          (insert (format "  - %s\n" (buffer-name buffer)))))
      (insert "\n==============================================\n"))
    (display-buffer claude-buffer)))

(defun claude-unified-chat ()
  "Start unified Claude experience - choose between chat and code modes."
  (interactive)
  (let ((choice (read-char-choice 
                 "Claude Mode: (c)hat, (t)erminal, (p)roject, (s)tatus: "
                 '(?c ?t ?p ?s))))
    (pcase choice
      (?c (gptel))
      (?t (claude-code-terminal))
      (?p (claude-code-project))
      (?s (claude-project-status)))))

(defun claude-send-region-to-terminal ()
  "Send selected region to active Claude Code terminal."
  (interactive)
  (if (use-region-p)
      (when (and claude-terminal-buffer
                 (buffer-live-p claude-terminal-buffer))
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (with-current-buffer claude-terminal-buffer
            (eat-term-send-string eat-terminal (concat text "\n")))))
    (message "No region selected")))

;;----------------------------------------------------------------------------
;; Export module
;;----------------------------------------------------------------------------
(provide 'config-claude)

;;; config-claude.el ends here