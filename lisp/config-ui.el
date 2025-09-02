;; User Interface  -*- lexical-binding: t; -*-
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines t)

;; Better scrolling behavior
(setq scroll-conservatively 101)
(setq scroll-margin 4)

;;------------------------------------------------------------------------------
;; Font Configuration
;;------------------------------------------------------------------------------
(set-face-attribute 'default nil
                    :font "JetBrainsMono Nerd Font"
                    :weight 'semi-bold
                    :height 120) ; 14 point = 140 in height units
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

;;------------------------------------------------------------------------------
;; Programming
;;------------------------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Custom faces for rainbow-delimiters
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#8BE9FD"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#50FA7B"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FFB86C"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#FF79C6"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#BD93F9"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#FF5555"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#F1FA8C"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#6272A4"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#E6E6E6")))))

;;------------------------------------------------------------------------------
;; Status Line
;;------------------------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(provide 'config-ui)





