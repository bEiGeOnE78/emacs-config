;;; test-claude-comprehensive.el --- Comprehensive test suite for Claude integration

;;; Commentary:
;; This file contains comprehensive tests for Phase 1 Claude integration
;; Tests cover configuration, API connection, all functions, and error handling

;;; Code:

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load required modules
(require 'config-packages)
(require 'config-claude)

;; Test results tracking
(defvar claude-test-results '()
  "List to store test results.")

(defvar claude-test-count 0
  "Counter for test number.")

(defun claude-test-assert (condition test-name &optional details)
  "Assert CONDITION is true for TEST-NAME with optional DETAILS."
  (setq claude-test-count (1+ claude-test-count))
  (let ((result (if condition "PASS" "FAIL"))
        (details-str (if details (format " - %s" details) "")))
    (push (format "Test %d: %s - %s%s" claude-test-count test-name result details-str)
          claude-test-results)
    (message "Test %d: %s - %s%s" claude-test-count test-name result details-str)
    condition))

(defun claude-test-summary ()
  "Print summary of all test results."
  (let ((passed (length (seq-filter (lambda (r) (string-match-p "PASS" r)) claude-test-results)))
        (failed (length (seq-filter (lambda (r) (string-match-p "FAIL" r)) claude-test-results))))
    (message "\n=== CLAUDE INTEGRATION TEST SUMMARY ===")
    (message "Total tests: %d" claude-test-count)
    (message "Passed: %d" passed)
    (message "Failed: %d" failed)
    (message "Success rate: %.1f%%" (* 100.0 (/ (float passed) claude-test-count)))
    (message "\nDetailed Results:")
    (dolist (result (reverse claude-test-results))
      (message "  %s" result))
    (message "========================================\n")))

;;----------------------------------------------------------------------------
;; Configuration Tests
;;----------------------------------------------------------------------------

(defun claude-test-configuration ()
  "Test basic configuration is correct."
  (message "\n--- Testing Configuration ---")
  
  ;; Test 1: gptel package loaded
  (claude-test-assert (featurep 'gptel) 
                      "gptel package loaded")
  
  ;; Test 2: gptel backend configured
  (claude-test-assert (and gptel-backend 
                           (gptel-backend-name gptel-backend))
                      "gptel backend configured"
                      (when gptel-backend (gptel-backend-name gptel-backend)))
  
  ;; Test 3: API key available
  (claude-test-assert (and gptel-api-key 
                           (stringp gptel-api-key)
                           (> (length gptel-api-key) 50))
                      "API key loaded"
                      (format "%d chars" (length (or gptel-api-key ""))))
  
  ;; Test 4: Model configuration
  (claude-test-assert (eq gptel-model 'claude-3-5-sonnet-20241022)
                      "Model correctly configured"
                      (symbol-name gptel-model))
  
  ;; Test 5: Directives configured
  (claude-test-assert (and gptel-directives 
                           (assq 'code-review gptel-directives))
                      "Custom directives configured"
                      (format "%d directives" (length gptel-directives))))

;;----------------------------------------------------------------------------
;; Key Binding Tests
;;----------------------------------------------------------------------------

(defun claude-test-key-bindings ()
  "Test all key bindings are correctly set."
  (message "\n--- Testing Key Bindings ---")
  
  ;; Test key bindings
  (claude-test-assert (eq (key-binding (kbd "C-c a c")) 'gptel)
                      "C-c a c bound to gptel")
  
  (claude-test-assert (eq (key-binding (kbd "C-c a s")) 'gptel-send)
                      "C-c a s bound to gptel-send")
  
  (claude-test-assert (eq (key-binding (kbd "C-c a r")) 'gptel-set-directive)
                      "C-c a r bound to gptel-set-directive")
  
  (claude-test-assert (eq (key-binding (kbd "C-c a k")) 'claude-reload-api-key)
                      "C-c a k bound to claude-reload-api-key")
  
  (claude-test-assert (eq (key-binding (kbd "C-c a d")) 'claude-debug-config)
                      "C-c a d bound to claude-debug-config"))

;;----------------------------------------------------------------------------
;; Function Tests
;;----------------------------------------------------------------------------

(defun claude-test-functions ()
  "Test all custom functions are available."
  (message "\n--- Testing Custom Functions ---")
  
  ;; Test custom function availability
  (claude-test-assert (fboundp 'claude-reload-api-key)
                      "claude-reload-api-key function available")
  
  (claude-test-assert (fboundp 'claude-debug-config)
                      "claude-debug-config function available")
  
  (claude-test-assert (fboundp 'claude-chat-code-review)
                      "claude-chat-code-review function available")
  
  (claude-test-assert (fboundp 'claude-chat-debugging)
                      "claude-chat-debugging function available")
  
  (claude-test-assert (fboundp 'claude-send-buffer-for-review)
                      "claude-send-buffer-for-review function available"))

;;----------------------------------------------------------------------------
;; Interactive Tests (require manual verification)
;;----------------------------------------------------------------------------

(defun claude-test-create-interactive-tests ()
  "Create buffer with interactive test instructions."
  (let ((test-buffer (get-buffer-create "*Claude Interactive Tests*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (insert "CLAUDE INTEGRATION - INTERACTIVE TESTS\n")
      (insert "=====================================\n\n")
      (insert "Please perform these tests manually in Emacs:\n\n")
      (insert "1. BASIC CHAT TEST:\n")
      (insert "   - Press C-c a c to start gptel\n")
      (insert "   - Type: 'What is 2+2?'\n")
      (insert "   - Press C-c RET to send\n")
      (insert "   - Expected: Claude responds with '4'\n")
      (insert "   - Result: [ ] PASS [ ] FAIL\n\n")
      
      (insert "2. SEND REGION TEST:\n")
      (insert "   - Create a new buffer with some code\n")
      (insert "   - Select a region of code\n")
      (insert "   - Press C-c a s to send to Claude\n")
      (insert "   - Expected: Claude analyzes the selected code\n")
      (insert "   - Result: [ ] PASS [ ] FAIL\n\n")
      
      (insert "3. DIRECTIVE TEST:\n")
      (insert "   - Press C-c a r to set directive\n")
      (insert "   - Choose 'code-review'\n")
      (insert "   - Start new chat with C-c a c\n")
      (insert "   - Expected: Chat uses code review directive\n")
      (insert "   - Result: [ ] PASS [ ] FAIL\n\n")
      
      (insert "4. CODE REVIEW FUNCTION TEST:\n")
      (insert "   - Run M-x claude-chat-code-review\n")
      (insert "   - Expected: Opens chat with code review directive\n")
      (insert "   - Result: [ ] PASS [ ] FAIL\n\n")
      
      (insert "5. DEBUGGING FUNCTION TEST:\n")
      (insert "   - Run M-x claude-chat-debugging\n")
      (insert "   - Expected: Opens chat with debugging directive\n")
      (insert "   - Result: [ ] PASS [ ] FAIL\n\n")
      
      (insert "6. BUFFER REVIEW TEST:\n")
      (insert "   - Open a code file\n")
      (insert "   - Run M-x claude-send-buffer-for-review\n")
      (insert "   - Expected: Sends entire buffer for code review\n")
      (insert "   - Result: [ ] PASS [ ] FAIL\n\n")
      
      (insert "7. API KEY RELOAD TEST:\n")
      (insert "   - Press C-c a k to reload API key\n")
      (insert "   - Expected: Shows API key status in minibuffer\n")
      (insert "   - Result: [ ] PASS [ ] FAIL\n\n")
      
      (insert "8. DEBUG CONFIG TEST:\n")
      (insert "   - Press C-c a d to show debug info\n")
      (insert "   - Expected: Shows configuration details in minibuffer\n")
      (insert "   - Result: [ ] PASS [ ] FAIL\n\n")
      
      (insert "NOTES:\n")
      (insert "- All tests should complete without authentication errors\n")
      (insert "- Claude responses should be relevant and helpful\n")
      (insert "- Any 401 authentication errors indicate API key problems\n")
      (insert "- Any network errors may indicate connectivity issues\n\n")
      
      (insert "After completing tests, run:\n")
      (insert "M-x claude-test-connection-live\n")
      (insert "for an automated connection test.\n"))
    (switch-to-buffer test-buffer)
    (goto-char (point-min))
    (message "Interactive test checklist created in *Claude Interactive Tests* buffer")))

;;----------------------------------------------------------------------------
;; Live Connection Test
;;----------------------------------------------------------------------------

(defun claude-test-connection-live ()
  "Test live connection to Claude API."
  (interactive)
  (message "Testing live Claude API connection...")
  (let ((test-buffer (get-buffer-create "*Claude Connection Test*"))
        (test-complete nil))
    (with-current-buffer test-buffer
      (erase-buffer)
      (insert "Testing Claude API connection...\n\n")
      (insert "Prompt: What is the capital of France? Answer in one word.\n\n")
      (insert "Response: "))
    
    ;; Send test request
    (gptel-request "What is the capital of France? Answer in one word."
                   :buffer test-buffer
                   :callback (lambda (response info)
                               (with-current-buffer test-buffer
                                 (goto-char (point-max))
                                 (if response
                                     (progn
                                       (insert response)
                                       (insert "\n\n✓ CONNECTION TEST PASSED")
                                       (message "Claude connection test: PASSED"))
                                   (progn
                                     (insert (format "ERROR: %s" info))
                                     (insert "\n\n✗ CONNECTION TEST FAILED")
                                     (message "Claude connection test: FAILED - %s" info))))))
    
    (switch-to-buffer test-buffer)
    (message "Connection test sent. Check *Claude Connection Test* buffer for results.")))

;;----------------------------------------------------------------------------
;; Main Test Runner
;;----------------------------------------------------------------------------

(defun claude-run-all-tests ()
  "Run all automated tests for Claude integration."
  (interactive)
  (message "Starting Claude integration test suite...")
  (setq claude-test-results '())
  (setq claude-test-count 0)
  
  ;; Run all test suites
  (claude-test-configuration)
  (claude-test-key-bindings)
  (claude-test-functions)
  
  ;; Show summary
  (claude-test-summary)
  
  ;; Create interactive test buffer
  (claude-test-create-interactive-tests)
  
  ;; Offer to run live test (only in interactive mode)
  (when (and (called-interactively-p 'any)
             (y-or-n-p "Run live API connection test now? "))
    (claude-test-connection-live))
  
  (message "Test suite completed. Check results above and complete interactive tests."))

;; Run tests when loaded in batch mode
(unless (called-interactively-p 'any)
  (claude-run-all-tests))

;;; Provide module
(provide 'test-claude-comprehensive)

;;; test-claude-comprehensive.el ends here