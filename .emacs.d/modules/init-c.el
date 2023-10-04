;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; C/C++ configuration.
;;

;;; Code:

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("<f12>" . compile))
  :init (setq-default c-basic-offset 4))

(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
