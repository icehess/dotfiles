;; init-erlang.el --- Initialize erlang configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Erlang configurations.
;;

;;; Code:

(use-package erlang
  :mode ("\\.erlang\\'" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("relx\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode))

(provide 'init-erlang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-docker.el ends here
