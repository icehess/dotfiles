;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Window configurations.
;;

;;; Code:

;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :hook (after-init . (lambda ()
                        (windmove-default-keybindings 'super))))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;; Quickly switch windows
(use-package ace-window
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :bind (([remap other-window] . ace-window))
  :hook (emacs-startup . ace-window-display-mode))

;; Enforce rules for popups
(use-package popper
  :defines popper-echo-dispatch-actions
  :autoload popper-group-by-directory
  :bind (:map popper-mode-map
         ("C-h z"     . popper-toggle-latest)
         ("C-<tab>"   . popper-cycle)
         ("C-M-<tab>" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-group-function #'popper-group-by-directory)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"
          "\\*Go-Translate\\*"
          "\\*Embark \\(Collect\\|Live\\):.*\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*eshell.*\\*.*$"       eshell-mode
          "^\\*shell.*\\*.*$"        shell-mode
          "^\\*terminal.*\\*.*$"     term-mode
          "^\\*vterm[inal]*.*\\*.*$" vterm-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))

  (setq popper-echo-dispatch-actions t)
  :config
  (popper-echo-mode 1)

  (with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
