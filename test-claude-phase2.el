;;; test-claude-phase2.el --- Test Phase 2 claude-code-ide integration

;;; Commentary:
;; Test suite for Phase 2 Claude Code IDE integration with MCP

;;; Code:

(defvar test-claude-phase2-results '()
  "Store test results for Phase 2.")

(defun test-claude-phase2-clear-results ()
  "Clear test results."
  (setq test-claude-phase2-results '()))

(defun test-claude-phase2-record (test-name result details)
  "Record a test result."
  (push (list test-name result details) test-claude-phase2-results))

(defun test-claude-phase2-display-results ()
  "Display all test results."
  (let ((buffer (get-buffer-create "*Claude Phase 2 Test Results*"))
        (passed 0)
        (failed 0))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== Claude Phase 2 Integration Test Results ===\n\n")
      
      (dolist (result (reverse test-claude-phase2-results))
        (let ((test-name (car result))
              (status (cadr result))
              (details (caddr result)))
          (if (eq status 'pass)
              (progn
                (insert (format "✅ %s\n" test-name))
                (setq passed (1+ passed)))
            (progn
              (insert (format "❌ %s\n" test-name))
              (setq failed (1+ failed))))
          (when details
            (insert (format "   %s\n" details)))))
      
      (insert (format "\nSummary: %d passed, %d failed\n" passed failed))
      (insert "===============================================\n"))
    
    (display-buffer buffer)))

;;----------------------------------------------------------------------------
;; Automated Tests for Phase 2
;;----------------------------------------------------------------------------

(defun test-claude-phase2-01-claude-cli-available ()
  "Test if Claude CLI is available in PATH."
  (let ((claude-path (executable-find "claude")))
    (if claude-path
        (test-claude-phase2-record "Claude CLI Available" 'pass 
                                   (format "Found at: %s" claude-path))
      (test-claude-phase2-record "Claude CLI Available" 'fail 
                                 "Claude CLI not found in PATH"))))

(defun test-claude-phase2-02-claude-cli-version ()
  "Test Claude CLI version command."
  (condition-case err
      (with-temp-buffer
        (if (eq 0 (call-process "claude" nil t nil "--version"))
            (let ((version (string-trim (buffer-string))))
              (test-claude-phase2-record "Claude CLI Version" 'pass version))
          (test-claude-phase2-record "Claude CLI Version" 'fail 
                                     "Failed to get version")))
    (error (test-claude-phase2-record "Claude CLI Version" 'fail 
                                      (format "Error: %s" err)))))

(defun test-claude-phase2-03-eat-package ()
  "Test if eat package is available."
  (condition-case err
      (progn
        (require 'eat)
        (test-claude-phase2-record "eat Package" 'pass "Package loaded successfully"))
    (error (test-claude-phase2-record "eat Package" 'fail 
                                      (format "Failed to load: %s" err)))))

(defun test-claude-phase2-04-claude-terminal-functions ()
  "Test if custom Claude terminal functions are available."
  (let ((functions '(claude-code-terminal
                     claude-code-project
                     claude-code-menu
                     claude-send-file-to-terminal
                     claude-execute-in-context))
        (missing '()))
    
    (dolist (func functions)
      (unless (fboundp func)
        (push (symbol-name func) missing)))
    
    (if missing
        (test-claude-phase2-record "Claude Terminal Functions" 'fail 
                                   (format "Missing: %s" (string-join missing ", ")))
      (test-claude-phase2-record "Claude Terminal Functions" 'pass 
                                 "All custom terminal functions defined"))))

(defun test-claude-phase2-05-project-detection ()
  "Test project detection."
  (let* ((project (project-current))
         (project-root (when project (project-root project))))
    (if project-root
        (test-claude-phase2-record "Project Detection" 'pass 
                                   (format "Detected: %s" project-root))
      (test-claude-phase2-record "Project Detection" 'fail 
                                 "No project detected"))))

(defun test-claude-phase2-06-key-bindings ()
  "Test if Phase 2 key bindings are defined."
  (let ((bindings '(("C-c a i" . claude-code-menu)
                    ("C-c a t" . claude-code-terminal)
                    ("C-c a p" . claude-code-project)
                    ("C-c a f" . claude-send-file-to-terminal)
                    ("C-c a x" . claude-execute-in-context)
                    ("C-c a g" . claude-send-region-to-terminal)
                    ("C-c a u" . claude-unified-chat)))
        (missing '()))
    
    (dolist (binding bindings)
      (let ((key (car binding))
            (command (cdr binding)))
        (unless (eq (key-binding (kbd key)) command)
          (push (format "%s -> %s" key command) missing))))
    
    (if missing
        (test-claude-phase2-record "Key Bindings" 'fail 
                                   (format "Missing: %s" (string-join missing ", ")))
      (test-claude-phase2-record "Key Bindings" 'pass 
                                 "All Phase 2 key bindings defined"))))

(defun test-claude-phase2-07-helper-functions ()
  "Test if helper functions are defined."
  (let ((functions '(claude-project-status
                     claude-unified-chat
                     claude-send-region-to-terminal))
        (missing '()))
    
    (dolist (func functions)
      (unless (fboundp func)
        (push (symbol-name func) missing)))
    
    (if missing
        (test-claude-phase2-record "Helper Functions" 'fail 
                                   (format "Missing: %s" (string-join missing ", ")))
      (test-claude-phase2-record "Helper Functions" 'pass 
                                 "All helper functions defined"))))

(defun test-claude-phase2-08-configuration-variables ()
  "Test if configuration variables are set."
  (let ((vars '((eat-term-name . "xterm-256color")
                (eat-kill-buffer-on-exit . t)))
        (incorrect '()))
    
    (dolist (var-pair vars)
      (let ((var (car var-pair))
            (expected (cdr var-pair)))
        (unless (and (boundp var) (equal (symbol-value var) expected))
          (push (format "%s = %s (expected %s)" 
                        var 
                        (if (boundp var) (symbol-value var) "unbound")
                        expected) 
                incorrect))))
    
    (if incorrect
        (test-claude-phase2-record "Configuration Variables" 'fail 
                                   (format "Issues: %s" (string-join incorrect "; ")))
      (test-claude-phase2-record "Configuration Variables" 'pass 
                                 "All variables configured correctly"))))

;;----------------------------------------------------------------------------
;; Test Runner
;;----------------------------------------------------------------------------

(defun test-claude-phase2-run-all ()
  "Run all Phase 2 tests."
  (interactive)
  (test-claude-phase2-clear-results)
  (message "Running Claude Phase 2 tests...")
  
  ;; Run all tests
  (test-claude-phase2-01-claude-cli-available)
  (test-claude-phase2-02-claude-cli-version)
  (test-claude-phase2-03-eat-package)
  (test-claude-phase2-04-claude-terminal-functions)
  (test-claude-phase2-05-project-detection)
  (test-claude-phase2-06-key-bindings)
  (test-claude-phase2-07-helper-functions)
  (test-claude-phase2-08-configuration-variables)
  
  ;; Display results
  (test-claude-phase2-display-results)
  (message "Phase 2 tests completed. Check results buffer."))

;;----------------------------------------------------------------------------
;; Interactive Tests
;;----------------------------------------------------------------------------

(defun test-claude-phase2-interactive ()
  "Interactive tests for Phase 2 that require user confirmation."
  (interactive)
  (let ((buffer (get-buffer-create "*Claude Phase 2 Interactive Tests*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== Claude Phase 2 Interactive Test Checklist ===\n\n")
      (insert "Please manually test the following:\n\n")
      
      (insert "1. □ C-c a u - Unified Claude experience menu works\n")
      (insert "2. □ C-c a t - Claude Code terminal opens\n")
      (insert "3. □ C-c a p - Project context is loaded\n")
      (insert "4. □ C-c a i - Interactive menu displays\n")
      (insert "5. □ C-c a f - Current file can be sent\n")
      (insert "6. □ Terminal displays in side window\n")
      (insert "7. □ Project detection works correctly\n")
      (insert "8. □ MCP tools integration functions\n")
      (insert "9. □ Claude responds to queries\n")
      (insert "10. □ Window management (resize/position) works\n\n")
      
      (insert "Key Binding Reference:\n")
      (insert "  Phase 1 (Chat):     C-c a c, s, r, k, d\n")
      (insert "  Phase 2 (Terminal): C-c a i, t, p, f, x, g\n")
      (insert "  Unified:            C-c a u\n\n")
      
      (insert "To test a specific function:\n")
      (insert "  M-x claude-project-status\n")
      (insert "  M-x claude-unified-chat\n")
      (insert "  M-x claude-send-region-to-terminal\n\n")
      
      (insert "================================================\n"))
    
    (display-buffer buffer)))

(message "Phase 2 test functions loaded:")
(message "- M-x test-claude-phase2-run-all     : Run automated tests")
(message "- M-x test-claude-phase2-interactive : Show interactive checklist")

(provide 'test-claude-phase2)

;;; test-claude-phase2.el ends here