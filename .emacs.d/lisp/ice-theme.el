(eval-when-compile
  (require 'init-funcs))

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

(when *is-a-mac*
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (if (display-graphic-p)
                  (menu-bar-mode 1)
                (menu-bar-mode -1))))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))


;; Make certain buffers grossly incandescent
(use-package solaire-mode
  :hook (after-load-theme . solaire-global-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; WORKAROUND: Visual bell on 29+
  ;; @see https://github.com/doomemacs/themes/issues/733
  ;; (with-no-warnings
  ;;   (defun my-doom-themes-visual-bell-fn ()
  ;;     "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  ;;     (let ((buf (current-buffer))
  ;;           (cookies (mapcar (lambda (face)
  ;;                              (face-remap-add-relative face 'doom-themes-visual-bell))
  ;;                            '(mode-line mode-line-active))))
  ;;       (force-mode-line-update)
  ;;       (run-with-timer 0.15 nil
  ;;                       (lambda ()
  ;;                         (with-current-buffer buf
  ;;                           (mapc #'face-remap-remove-relative cookies)
  ;;                           (force-mode-line-update))))))
  ;;   (advice-add #'doom-themes-visual-bell-fn :override #'my-doom-themes-visual-bell-fn))
  )

(set-face-attribute 'default nil
                    :family     "FantasqueSansM Nerd Font Mono"
                    :height     130)

;; Easily adjust the font size in all frames
;; which binds C-M-= and C-M-- and C-M-0 by default
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode))

;; Child frame
(when (childframe-workable-p)
  (use-package posframe
    :hook (after-load-theme . posframe-delete-all)
    :init
    (defface posframe-border
      `((t (:inherit region)))
      "Face used by the `posframe' border."
      :group 'posframe)
    (defvar posframe-border-width 2
      "Default posframe border width.")
    :config
    (with-no-warnings
      (defun my-posframe--prettify-frame (&rest _)
        (set-face-background 'fringe nil posframe--frame))
      (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

      (defun posframe-poshandler-frame-center-near-bottom (info)
        (cons (/ (- (plist-get info :parent-frame-width)
                    (plist-get info :posframe-width))
                 2)
              (/ (+ (plist-get info :parent-frame-height)
                    (* 2 (plist-get info :font-height)))
                 2))))))

(provide 'ice-theme)
