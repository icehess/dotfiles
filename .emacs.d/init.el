(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	'("elpa" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	'("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;	     '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
;; (add-to-list 'package-archives
;;	     '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
(package-initialize)
;(package-refresh-contents)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(if (file-exists-p (expand-file-name "config-editor.org" user-emacs-directory))
    (org-babel-load-file (expand-file-name "config-editor.org" user-emacs-directory)))

;; interface tweaks
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'iceberg t)

;;(use-package nord-theme
;;  :ensure t
;;  :config (load-theme 'nord t))

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(save-place-mode 1)

(xterm-mouse-mode t)
(pixel-scroll-mode t)
(unless window-system
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(global-display-line-numbers-mode 1)
(line-number-mode 1)

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

;; Store all backup and autosave files in the tmp dir
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)

;; Do not create lockfiles - I’m the only user
;; I'm not sure about the rationale behind this setting, but the auto-generated files are an annoyance, so they walk the plank.
(setq create-lockfiles nil)


;; JAMES

(use-package erlang
  :ensure t
  :config
  ;; formats the buffer before saving
  (add-hook 'erlang-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'erlang-indent-current-buffer nil 'make-it-local)
            ))
  )
;; Install the yasnippet dependency
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  )

;; Customize prefix for key-bindings
(setq lsp-keymap-prefix "C-c l")

;; ----- lsp-mode -----
;; Install the official lsp-mode package (minimum required version 6.2)
(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
          (erlang-mode . lsp)
          ;; if you want which-key integration
          (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-log-io t)
  ; :config
  ; ;; Enable LSP automatically for Erlang files
  ; (add-hook 'erlang-mode-hook #'lsp)
  )


;; Show line and column numbers
;(add-hook 'erlang-mode-hook 'linum-mode)
;(add-hook 'erlang-mode-hook 'column-number-mode)

;; ----- lsp-ui -----
;; It is usually a good idea to install lsp-ui as well
(use-package lsp-ui
  :ensure t
  :config
  ;; The lsp-ui sideline can become a serious distraction, so you
  ;; may want to disable it
  (setq lsp-ui-sideline-enable nil)
  ;; Ensure docs are visible
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom)
  )

;; ----- company-lsp -----
;; Enables better integration with company (auto-completion)
(use-package company-lsp
  :ensure t
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-lsp))
  )

(use-package ivy
    :ensure t
    :diminish (ivy-mode)
    :bind (("C-x b" . ivy-switch-buffer))
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "%d/%d ")
    (setq ivy-display-style 'fancy))

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; ----- helm-lsp -----
;; Provides commands to list workspace symbols:
;; - helm-lsp-workspace-symbol
;; - helm-lsp-global-workspace-symbol
;(use-package helm-lsp)

;; ----- lsp-origami -----
;; Be able to fold ranges via origami.el
(use-package lsp-origami
  :ensure t
  :config
  (add-hook 'origami-mode-hook #'lsp-origami-mode)
  (add-hook 'erlang-mode-hook #'origami-mode)
  )

(use-package lsp-mode
  :hook (erlang-mode . lsp-deferred)
  :commands lsp
  )

;; Which-key integration
(use-package which-key
  :ensure t
  :config
  (add-hook 'erlang-mode-hook 'which-key-mode)
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    )
)

;; always show diagnostics at the bottom, using 1/3 of the available space
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.33)))






















(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("28c928e0dca7617ba666c429183ceb11c582d2d2977a646ac7a0985513799e53" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))
 '(package-selected-packages
   '(edts dockerfile-mode smart-mode-line counsel ace-window editorconfig erlang try which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
