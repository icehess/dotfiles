;;; init-ocaml.el --- Support the OCaml language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tuareg
  :config
  (defvar-local tuareg-previous-tuareg-buffer nil
    "Buffer from which we jumped to the REPL.")

  (defun sanityinc/tuareg-repl-switch ()
    (interactive)
    (let ((last-tuareg-buf (when (derived-mode-p 'tuareg-mode)
                             (current-buffer))))
      (tuareg-run-ocaml)
      (pop-to-buffer tuareg-interactive-buffer-name)
      (when last-tuareg-buf
        (setq-local tuareg-previous-tuareg-buffer last-tuareg-buf))))

  (defun sanityinc/tuareg-repl-switch-back ()
    (interactive)
    (when tuareg-previous-tuareg-buffer
      (pop-to-buffer tuareg-previous-tuareg-buffer)))

  (define-key tuareg-mode-map (kbd "C-c C-z") 'sanityinc/tuareg-repl-switch)
  (define-key tuareg-interactive-mode-map (kbd "C-c C-z") 'sanityinc/tuareg-repl-switch-back))

(when (maybe-require-package 'dune)
  (maybe-require-package 'dune-format))

(provide 'init-ocaml)
;;; init-ocaml.el ends here
