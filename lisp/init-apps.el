;;; init-apps.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)

 :custom
  (gptel-model "gpt-3.5-turbo")
  )

(use-package docker
  :bind ("C-c d" . docker))

(provide 'init-apps)
;;; init-apps.el ends here
