;;; config-proxy.el --- Automatic proxy detection and management

;;; Commentary:
;; This file provides automatic proxy detection and management for Emacs.
;; It can detect network changes and prompt to switch between proxy profiles.

;;; Code:

;;----------------------------------------------------------------------------
;; Proxy Configuration Storage
;;----------------------------------------------------------------------------

(defcustom my/proxy-profiles
  '((none
     :name "No Proxy"
     :http-proxy nil
     :https-proxy nil
     :ftp-proxy nil
     :no-proxy-list '("localhost" "127.0.0.1" "::1"))
    
    (corporate
     :name "Corporate Proxy"
     :http-proxy "http://proxy.company.com:8080"
     :https-proxy "http://proxy.company.com:8080" 
     :ftp-proxy "http://proxy.company.com:8080"
     :no-proxy-list '("localhost" "127.0.0.1" "::1" "*.company.com" "10.*" "192.168.*"))
    
    (home
     :name "Home Network"
     :http-proxy nil
     :https-proxy nil
     :ftp-proxy nil
     :no-proxy-list '("localhost" "127.0.0.1" "::1")))
  "Proxy profile configurations.
Each profile is a plist with :name, :http-proxy, :https-proxy, :ftp-proxy, and :no-proxy-list."
  :type 'list
  :group 'my-proxy)

(defvar my/current-proxy-profile 'none
  "Current active proxy profile.")

(defvar my/last-network-check-time nil
  "Time of last network check.")

(defvar my/network-check-interval 300
  "Interval in seconds between automatic network checks.")

;;----------------------------------------------------------------------------
;; Network Detection Functions
;;----------------------------------------------------------------------------

(defun my/detect-network-environment ()
  "Detect current network environment and suggest appropriate proxy."
  (let* ((default-gateway (my/get-default-gateway))
         (network-name (my/get-network-name))
         (dns-servers (my/get-dns-servers)))
    
    (cond
     ;; Corporate network detection patterns
     ((or (string-match-p "company\\.com" (or network-name ""))
          (string-match-p "^10\\." (or default-gateway ""))
          (string-match-p "corp" (or network-name "")))
      'corporate)
     
     ;; Home network patterns
     ((or (string-match-p "^192\\.168\\." (or default-gateway ""))
          (string-match-p "home" (or network-name "")))
      'home)
     
     ;; Default to no proxy
     (t 'none))))

(defun my/get-default-gateway ()
  "Get the default gateway IP address."
  (condition-case nil
      (with-temp-buffer
        (call-process "ip" nil t nil "route" "show" "default")
        (goto-char (point-min))
        (when (re-search-forward "via \\([0-9.]+\\)" nil t)
          (match-string 1)))
    (error nil)))

(defun my/get-network-name ()
  "Get current network/WiFi name."
  (condition-case nil
      (with-temp-buffer
        ;; Try nmcli first (NetworkManager)
        (if (= 0 (call-process "nmcli" nil t nil "-t" "-f" "active,ssid" "dev" "wifi"))
            (progn
              (goto-char (point-min))
              (when (re-search-forward "yes:\\(.+\\)" nil t)
                (match-string 1)))
          ;; Fallback to iwconfig
          (erase-buffer)
          (call-process "iwconfig" nil t nil)
          (goto-char (point-min))
          (when (re-search-forward "ESSID:\"\\([^\"]+\\)\"" nil t)
            (match-string 1))))
    (error nil)))

(defun my/get-dns-servers ()
  "Get current DNS server addresses."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents "/etc/resolv.conf")
        (let ((dns-servers '()))
          (goto-char (point-min))
          (while (re-search-forward "^nameserver \\([0-9.]+\\)" nil t)
            (push (match-string 1) dns-servers))
          (nreverse dns-servers)))
    (error nil)))

;;----------------------------------------------------------------------------
;; Proxy Management Functions
;;----------------------------------------------------------------------------

(defun my/apply-proxy-profile (profile-name)
  "Apply the specified proxy profile."
  (interactive 
   (list (intern (completing-read "Select proxy profile: " 
                                  (mapcar (lambda (p) (symbol-name (car p))) my/proxy-profiles)
                                  nil t))))
  
  (let* ((profile (cdr (assoc profile-name my/proxy-profiles)))
         (http-proxy (plist-get profile :http-proxy))
         (https-proxy (plist-get profile :https-proxy))
         (ftp-proxy (plist-get profile :ftp-proxy))
         (no-proxy-list (plist-get profile :no-proxy-list)))
    
    ;; Set environment variables
    (if http-proxy
        (setenv "http_proxy" http-proxy)
      (setenv "http_proxy" nil))
    
    (if https-proxy
        (setenv "https_proxy" https-proxy)
      (setenv "https_proxy" nil))
    
    (if ftp-proxy
        (setenv "ftp_proxy" ftp-proxy)
      (setenv "ftp_proxy" nil))
    
    (if no-proxy-list
        (setenv "no_proxy" (string-join no-proxy-list ","))
      (setenv "no_proxy" nil))
    
    ;; Configure Emacs-specific proxy settings
    (if http-proxy
        (progn
          (setq url-proxy-services
                `(("http" . ,(replace-regexp-in-string "^https?://" "" http-proxy))
                  ("https" . ,(replace-regexp-in-string "^https?://" "" https-proxy))
                  ("ftp" . ,(replace-regexp-in-string "^https?://" "" ftp-proxy))))
          (setq url-gateway-method 'native))
      (progn
        (setq url-proxy-services nil)
        (setq url-gateway-method 'native)))
    
    (setq my/current-proxy-profile profile-name)
    (message "Applied proxy profile: %s" (plist-get profile :name))))

(defun my/check-and-suggest-proxy ()
  "Check network environment and suggest proxy changes if needed."
  (interactive)
  (let ((detected-profile (my/detect-network-environment)))
    (when (and detected-profile 
               (not (eq detected-profile my/current-proxy-profile)))
      (when (y-or-n-p (format "Network change detected. Switch to %s proxy profile? " 
                              (plist-get (cdr (assoc detected-profile my/proxy-profiles)) :name)))
        (my/apply-proxy-profile detected-profile)))))

(defun my/automatic-proxy-check ()
  "Automatically check network and suggest proxy changes periodically."
  (when (or (null my/last-network-check-time)
            (> (- (float-time) my/last-network-check-time) my/network-check-interval))
    (setq my/last-network-check-time (float-time))
    (my/check-and-suggest-proxy)))

;;----------------------------------------------------------------------------
;; Proxy Status and Information
;;----------------------------------------------------------------------------

(defun my/show-proxy-status ()
  "Show current proxy configuration status."
  (interactive)
  (let* ((profile (cdr (assoc my/current-proxy-profile my/proxy-profiles)))
         (profile-name (plist-get profile :name))
         (http-proxy (or (getenv "http_proxy") "Not set"))
         (https-proxy (or (getenv "https_proxy") "Not set"))
         (no-proxy (or (getenv "no_proxy") "Not set")))
    
    (message "Proxy Status - Profile: %s | HTTP: %s | HTTPS: %s | No-proxy: %s" 
             profile-name http-proxy https-proxy no-proxy)))

(defun my/test-proxy-connection ()
  "Test current proxy configuration by attempting to connect to a test URL."
  (interactive)
  (message "Testing proxy connection...")
  (url-retrieve "https://httpbin.org/ip"
                (lambda (status)
                  (if (plist-get status :error)
                      (message "Proxy test failed: %s" (plist-get status :error))
                    (message "Proxy connection successful")))
                nil t))

;;----------------------------------------------------------------------------
;; Integration with Package Management
;;----------------------------------------------------------------------------

(defun my/configure-package-proxy ()
  "Configure package.el to use current proxy settings."
  (when (and (boundp 'url-proxy-services) url-proxy-services)
    (setq package-check-signature nil) ;; May be needed behind corporate proxies
    (message "Package manager configured for proxy use")))

;; Hook into package initialization
(add-hook 'package-initialize-hook 'my/configure-package-proxy)

;;----------------------------------------------------------------------------
;; Automatic Network Monitoring
;;----------------------------------------------------------------------------

(defvar my/proxy-timer nil
  "Timer for automatic proxy checking.")

(defun my/start-proxy-monitoring ()
  "Start automatic proxy monitoring."
  (interactive)
  (when my/proxy-timer
    (cancel-timer my/proxy-timer))
  (setq my/proxy-timer 
        (run-with-timer 0 my/network-check-interval 'my/automatic-proxy-check))
  (message "Proxy monitoring started"))

(defun my/stop-proxy-monitoring ()
  "Stop automatic proxy monitoring."
  (interactive)
  (when my/proxy-timer
    (cancel-timer my/proxy-timer)
    (setq my/proxy-timer nil)
    (message "Proxy monitoring stopped")))

;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------

(define-prefix-command 'my/proxy-map)
(global-set-key (kbd "C-c p") 'my/proxy-map)

(define-key my/proxy-map (kbd "s") 'my/apply-proxy-profile)
(define-key my/proxy-map (kbd "c") 'my/check-and-suggest-proxy)
(define-key my/proxy-map (kbd "i") 'my/show-proxy-status)
(define-key my/proxy-map (kbd "t") 'my/test-proxy-connection)
(define-key my/proxy-map (kbd "m") 'my/start-proxy-monitoring)
(define-key my/proxy-map (kbd "q") 'my/stop-proxy-monitoring)

;; Integration with which-key
(with-eval-after-load 'which-key
  (add-to-list 'which-key-replacement-alist
               '(("C-c p" . "Prefix Command") . ("C-c p" . "proxy"))))

;;----------------------------------------------------------------------------
;; Initialization
;;----------------------------------------------------------------------------

;; Start with automatic network detection
(add-hook 'after-init-hook 
          (lambda ()
            (run-with-timer 2 nil 'my/check-and-suggest-proxy)
            (my/start-proxy-monitoring)))

;;----------------------------------------------------------------------------
;; Export module
;;----------------------------------------------------------------------------
(provide 'config-proxy)

;;; config-proxy.el ends here