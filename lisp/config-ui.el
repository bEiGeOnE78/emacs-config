;; User Interface
(tool-bar-mode -1)
(menu-bar-mode -1)

;;------------------------------------------------------------------------------
;; Font Configuration
;;------------------------------------------------------------------------------
(set-face-attribute 'default nil
                    :font "JetBrainsMono Nerd Font"
                    :weight 'semi-bold
                    :height 140) ; 14 point = 140 in height units
(use-package display-line-numbers
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

(use-package olivetti
  :ensure t
  :commands olivetti-mode
  :config
  (setq olivetti-set-width 100)
  (setq olivetti-style nil)) ;; Use margins instead of fringes

;;------------------------------------------------------------------------------
;; Reading Mode Configuration
;;------------------------------------------------------------------------------
(defun my/reading-mode ()
  "Toggle reading mode for distraction-free reading of org and markdown files."
  (interactive)
  (if olivetti-mode
      (progn
        (olivetti-mode -1)
        (display-line-numbers-mode 1)
        (message "Reading mode disabled"))
    (progn
      (olivetti-mode 1)
      (display-line-numbers-mode -1)
      (message "Reading mode enabled"))))

;; Automatically enable reading mode for org and markdown files
(defun my/maybe-enable-reading-mode ()
  "Automatically enable reading mode for org and markdown files."
  (when (or (derived-mode-p 'org-mode)
            (derived-mode-p 'markdown-mode)
            (derived-mode-p 'gfm-mode))
    (olivetti-mode 1)
    (display-line-numbers-mode -1)))

;; Hook for automatic reading mode
(add-hook 'find-file-hook 'my/maybe-enable-reading-mode)

;; Global key binding for manual reading mode toggle
(global-set-key (kbd "C-c r") 'my/reading-mode)
;;------------------------------------------------------------------------------
;; Dashboard
;;------------------------------------------------------------------------------
(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Vader has entered the chat!")
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

(use-package nerd-icons
  :ensure t)

;; Use M-p, M-n to "scroll" (actually, move lines)
(defun my/next-line () (interactive) (next-line 10))
(defun my/previous-line () (interactive) (previous-line 10))
(keymap-global-set "M-p" #'my/previous-line)
(keymap-global-set "M-n" #'my/next-line)

;;------------------------------------------------------------------------------
;; Theme
;;------------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/dracula")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/eldritch-emacs")
(load-theme 'eldritch t)

(provide 'config-ui)
