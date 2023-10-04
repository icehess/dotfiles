;;; init-yaml.el --- Support Yaml files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :config
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))


(provide 'init-yaml)
;;; init-yaml.el ends here
