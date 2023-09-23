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

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(set-face-attribute 'default nil
                    :family     "FantasqueSansM Nerd Font Mono"
                    :height     150)

(provide 'ice-theme)
