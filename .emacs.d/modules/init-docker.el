;; init-docker.el --- Initialize docker configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Docker configurations.
;;

;;; Code:

;; Docker
(use-package docker
  :defines docker-image-run-arguments
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
              docker-container-shell-file-name "/bin/bash"))

;;`tramp-container' is builtin since 29
(use-package docker-tramp)

(use-package dockerfile-mode)
(use-package docker-compose-mode)

(provide 'init-docker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-docker.el ends here
