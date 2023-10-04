;; init-markdown.el --- Initialize markdown configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Markdown configurations.
;;

;;; Code:

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t)
  :config
  ;; Table of contents
  (use-package markdown-toc
    :diminish
    :bind (:map markdown-mode-command-map
           ("r" . markdown-toc-generate-or-refresh-toc))
    :hook (markdown-mode . markdown-toc-mode)
    :init (setq markdown-toc-indentation-space 2
                markdown-toc-header-toc-title "\n## Table of Contents"
                markdown-toc-user-toc-structure-manipulation-fn 'cdr)
    :config
    (with-no-warnings
      (define-advice markdown-toc-generate-toc (:around (fn &rest args) lsp)
        "Generate or refresh toc after disabling lsp."
        (cond
         ((bound-and-true-p lsp-managed-mode)
          (lsp-managed-mode -1)
          (apply fn args)
          (lsp-managed-mode 1))
         ((bound-and-true-p eglot--manage-mode)
          (eglot--manage-mode -1)
          (apply fn args)
          (eglot--manage-mode 1))
         (t
          (apply fn args)))))))

(provide 'init-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
