(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	'("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	'("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;	     '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
;; (add-to-list 'package-archives
;;	     '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
(package-initialize)

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

(use-package doom-themes
    :ensure t)

(use-package try
    :ensure t)

(use-package which-key
    :ensure t
    :config (which-key-mode))

(use-package powerline
    :ensure t)
;; :config
;; (powerline-center-theme))

(use-package smart-mode-line
    :ensure t
    :config
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'dark)
    (sml/setup))

(use-package dockerfile-mode
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;;(use-package xclip
;;    :config (xclip-mode 1))

(winner-mode 1)
;; more info: https://github.com/abo-abo/ace-window/wiki#manage-100-window-operations
(use-package ace-window
    :ensure t
    :defer 1
    :config
    (global-set-key (kbd "M-o") 'ace-window)
    (set-face-attribute
        'aw-leading-char-face nil
        :foreground "deep sky blue"
        :weight 'bold
        :height 3.0)
    (set-face-attribute
        'aw-mode-line-face nil
        :inherit 'mode-line-buffer-id
        :foreground "lawn green")
    (setq aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window "Ace - Delete Window")
             (?c aw-swap-window "Ace - Swap Window")
             (?n aw-flip-window)
             (?v aw-split-window-vert "Ace - Split Vert Window")
             (?h aw-split-window-horz "Ace - Split Horz Window")
             (?m delete-other-windows "Ace - Maximize Window")
             (?g delete-other-windows)
             (?b balance-windows)
             (?u (lambda ()
                     (progn
                         (winner-undo)
                         (setq this-command 'winner-undo))))
             (?r winner-redo)))

    (when (package-installed-p 'hydra)
        (defhydra hydra-window-size (:color red)
            "Windows size"
            ("h" shrink-window-horizontally "shrink horizontal")
            ("j" shrink-window "shrink vertical")
            ("k" enlarge-window "enlarge vertical")
            ("l" enlarge-window-horizontally "enlarge horizontal"))
        (defhydra hydra-window-frame (:color red)
            "Frame"
            ("f" make-frame "new frame")
            ("x" delete-frame "delete frame"))
        (defhydra hydra-window-scroll (:color red)
            "Scroll other window"
            ("n" joe-scroll-other-window "scroll")
            ("p" joe-scroll-other-window-down "scroll down"))
        (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
        (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
        (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
    (ace-window-display-mode t))

(use-package counsel
    :ensure t
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

(use-package swiper
    :ensure try
    :bind (("C-s" . swiper)
              ("C-c C-r" . ivy-resume)
              ("<f6>" . ivy-resume)
              ("M-x" . counsel-M-x)
              ("C-x C-f" . counsel-find-file)
              ("<f1> f" . counsel-describe-function)
              ("<f1> v" . counsel-describe-variable)
              ("<f1> l" . counsel-load-library)
              ("<f2> i" . counsel-info-lookup-symbol)
              ("<f2> u" . counsel-unicode-char)
              ("C-c g" . counsel-git)
              ("C-c j" . counsel-git-grep)
              ("C-c k" . counsel-ag)
              ("C-x l" . counsel-locate))
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1))

;; magit
(use-package magit
    :ensure t
    :config (setq magit-diff-refine-hunk 'all))

;; GitHub pull requests
;; (use-package magit-gh-pulls
;;     :ensure t
;;     :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

;; Git gutters
(use-package diff-hl
    :ensure t
    :init
    (setq diff-hl-side 'right)
    :config
    (global-diff-hl-mode 1)
    (diff-hl-margin-mode 1)
    (diff-hl-flydiff-mode 1))

(defalias 'list-buffers 'ibuffer)

(defconst icehess-otp_root
    (if (= (length (getenv "OTP_ROOT")) 0)
        "/usr/lib/erlang"
        (getenv "OTP_ROOT")))
(defconst icehess-kazoo-root
    (if (= (length (getenv "KAZOO_SRC")) 0)
        "~/work/2600hz/kazoo/master"
        (getenv "KAZOO_SRC")))
(use-package erlang
    :ensure t
    :config
    (progn
        (setq load-path (cons (expand-file-name (concat icehess-otp_root "/lib/tools-*/emacs"))
			                load-path))
        (setq erlang-root-dir icehess-otp_root)
        (setq exec-path (cons (concat icehess-otp_root "/bin")
                            exec-path))
        (require 'erlang-start)
        (add-hook 'erlang-mode-hook
            (lambda ()
                (setq mode-name "erl")
                (setq erlang-compile-extra-opts
                    (list
                        "-I./"
                        "-I../include"
                        (concat "-I" (expand-file-name (concat icehess-kazoo-root "/deps")))
                        (concat "-I" (expand-file-name (concat icehess-kazoo-root "/core")))
                        (concat "-I" (expand-file-name (concat icehess-kazoo-root "/applications")))
                        ))))))

(use-package edts
    :ensure t
    :init
    :config
    (require 'edts-start))

;; Web-mode: Initialize web-mode and recognize extensions. Also
;; consider the possibility of JSX files with a .js extension istead
;; of .jsx.
(use-package web-mode
    :ensure t
    :init
    (setq web-mode-content-types-alist '(("jsx" . "\\.tsx\\'")))
    (setq web-mode-content-types-alist '(("jsx" . "\\.js\\'")))
    :config
    (add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode)))

;;Add node_modules path
(use-package add-node-modules-path
    :ensure t)

;; Run prettier on save if web-mode
(eval-after-load 'web-mode
    '(progn
         (add-hook 'web-mode-hook #'add-node-modules-path)
         (add-hook 'web-mode-hook #'prettier-js-mode)))

;; GraphQL
(use-package graphql-mode
    :ensure t)

;; Yaml-mode
(use-package yaml-mode :ensure t)

;; Rust
(use-package rust-mode
    :ensure t)

;; Markdown
(use-package markdown-mode
    :ensure t
    :mode (("README\\.md\\'" . gfm-mode)
              ("\\.md\\'" . markdown-mode)
              ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

;; Flycheck
;; Flycheck is used for on-the-fly linting. We set the
;; indication mode to nil, because otherwise it conflicts with the
;; line numbers. This disables the indicators in the fringe, but
;; still shows the marked errors in the buffer. We set a zero delay
;; to show the error message on the status bar below, and set a 0.2
;; second delay to avoid machine-gunning eslint.
(use-package flycheck
    :ensure t
    :init
    ;; (setq flycheck-indication-mode nil)
    ;; (setq flycheck-display-errors-delay nil)
    ;; (setq flycheck-idle-change-delay 2)
    (setq flycheck-erlang-include-path
        (list "../include"
            (expand-file-name (concat icehess-kazoo-root "/deps"))
            (expand-file-name (concat icehess-kazoo-root "/core"))
            (expand-file-name (concat icehess-kazoo-root "/applications"))))
    (setq flycheck-erlang-library-path
        (list
            (expand-file-name (concat icehess-kazoo-root "/deps"))
            (expand-file-name (concat icehess-kazoo-root "/core"))
            (expand-file-name (concat icehess-kazoo-root "/applications"))
            ))
    (global-flycheck-mode))

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(custom-safe-themes
         (quote
             ("28c928e0dca7617ba666c429183ceb11c582d2d2977a646ac7a0985513799e53" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
    '(package-selected-packages
         (quote
             (edts dockerfile-mode smart-mode-line counsel ace-window editorconfig erlang try which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
