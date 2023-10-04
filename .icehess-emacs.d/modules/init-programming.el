;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish
  :config
  (when (childframe-workable-p)
    (use-package eldoc-box
      :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
      :custom-face
      (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
      (eldoc-box-body ((t (:inherit tooltip))))
      :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
      :config
      ;; Prettify `eldoc-box' frame
      (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
            (alist-get 'right-fringe eldoc-box-frame-parameters) 8))))


(use-package ag)
(use-package wgrep-ag)
(setq-default ag-highlight-search t)

(use-package rg)

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

(use-package cmake-mode)
(use-package lua-mode)
(use-package vimrc-mode)

(provide 'init-programming)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
