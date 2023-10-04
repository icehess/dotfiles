;; init-elixir.el --- Initialize elixir configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Elixir configurations.
;;

;;; Code:

(use-package elixir-mode
  :config
  (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
          '((elixir-mode elixir-ts-mode heex-ts-mode)
               "elixir-ls")))
  (use-package alchemist
    :diminish (alchemist-mode alchemist-phoenix-mode)
    :hook ((elixir-mode . alchemist-mode)
           (elixir-mode . alchemist-phoenix-mode))))

(provide 'init-elixir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elixir.el ends here
