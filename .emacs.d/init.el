;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
;; (require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))


;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Default layout
(require 'nano-layout)


;; Bootstrap config
(require 'init-funcs)
(require 'init-elpa)
(require 'nano-defaults)
(require 'nano-bindings)

;; Theme
(require 'ice-theme)
(require 'nano-splash)

(require 'init-window)

(require 'init-dired)
(require 'init-isearch)
(require 'init-ibuffer)
(require 'init-completion)
(require 'init-hippie-expand)
(require 'init-corfu)
(require 'init-sessions)
(require 'init-editing-utils)
(require 'init-whitespace)
(require 'init-treemacs)

(require 'init-projectile)

(require 'init-mmm)

(require 'init-flymake)
(require 'init-lsp)
(require 'init-vcs)
(require 'init-yasnippet)

(require 'init-docker)
(require 'init-markdown)
(require 'init-programming)

(require 'init-elisp)
(require 'init-erlang)
(require 'init-elixir)
(require 'init-c)
(require 'init-web)
(require 'init-php)
(require 'init-python)
(require 'init-ocaml)
(require 'init-rust)
(require 'init-toml)
(require 'init-yaml)
(require 'init-folding)
(require 'init-lua)

;; Extra packages which don't require any configuration

(use-package dotenv-mode)
(use-package shfmt)

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

;; 

;; ;; Allow access from emacsclient
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (require 'server)
;;             (unless (server-running-p)
;;               (server-start))))

;; Locales (setting them earlier in this file doesn't work in X)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
