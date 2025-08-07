;;; config-packages.el --- Package configuration using use-package

;;; Commentary:
;; This file contains the main package configurations for Emacs,
;; organized using the 'use-package' macro for clarity and performance.

;;; Code:

;;----------------------------------------------------------------------------
;; Package Management Setup
;;----------------------------------------------------------------------------

;; Initialize the built-in package manager
(require 'package)

;; Add package archives. MELPA is the most popular community-driven archive.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; You can add others here as well, like GNU ELPA or Org ELPA.
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; Initialize the package system. This must be done before any package operations.
(package-initialize)

;;----------------------------------------------------------------------------
;; Bootstrap use-package
;;----------------------------------------------------------------------------

;; This code ensures that 'use-package' is installed. If it's not,
;; it refreshes the package contents and installs it automatically.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package.
;; eval-when-compile is used to make the use-package macro available at compile-time.
(eval-when-compile
  (require 'use-package))

;; Set a global default for use-package to always ensure packages are installed.
;; This saves you from having to write :ensure t for every package.
(setq use-package-always-ensure t)

;;------------------------------------------------------------------------------
;; Load some packages
;;------------------------------------------------------------------------------
(use-package which-key
  :config
  (which-key-mode))

;;------------------------------------------------------------------------------
;; Export module
;;------------------------------------------------------------------------------
(provide 'config-packages)
