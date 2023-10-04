;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Web configurations.
;;

;;; Code:

;; CSS
(use-package css-mode
  :init (setq css-indent-offset 2)
  :config
  ;;; Colourise CSS colour literals
  (use-package rainbow-mode
    :config
    (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
      (add-hook hook 'rainbow-mode)))

  ;;; Embedding in html
  (with-eval-after-load 'mmm-vars
    (mmm-add-group
     'html-css
     '((css-cdata
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
        :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
        :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</style>" @)))
       (css
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t]*\n?"
        :back "[ \t]*</style>"
        :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</style>" @)))
       (css-inline
        :submode css-mode
        :face mmm-code-submode-face
        :front "style=\""
        :back "\"")))
    (dolist (mode (list 'html-mode 'nxml-mode))
      (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))))

;;; Use eldoc for syntax hints
(use-package css-eldoc
  :config
  (autoload 'turn-on-css-eldoc "css-eldoc")
  (add-hook 'css-mode-hook 'turn-on-css-eldoc))

;; SCSS
(use-package scss-mode
  :init (setq scss-compile-at-save nil))

;; LESS
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; JSON
(unless (fboundp 'js-json-mode)
  (use-package json-mode))

;; XML
(use-package nxml-mode
  :ensure nil
  :mode (("\\.\\(xaml\\|xml\\|xslt\\|svg\\|rss\\|plist\\)$" . xml-mode)))

;; JavaScript
(use-package js
  :init (setq js-indent-level 2))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  ;; Use default keybindings for lsp
  (unbind-key "M-." js2-mode-map))

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(when (executable-find "prettier")
  (use-package prettier
    :diminish
    :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none)))

(use-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\|[h]eex\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

;; REST
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode)))

(provide 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
