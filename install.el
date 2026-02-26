;;; install.el --- Package installation and initialization  -*- lexical-binding: t -*-

;;; Commentary:
;; This file handles Emacs package initialization and installation.
;; It configures package archives and provides functions for managing packages.
;;
;;; Code:

;; Add site-lisp to load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Initialize package system
(require 'package)

;; Disable package signature check (optional, for compatibility)
(setq package-check-signature nil)

;; Add MELPA repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize packages (standard initialization, no network calls here)
(package-initialize)

(provide 'my-install)
;;; install.el ends here
