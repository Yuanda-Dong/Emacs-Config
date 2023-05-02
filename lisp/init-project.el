;;; init-project.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; (use-package projectile
;;   :hook (after-init . projectile-mode)
;;   :bind (:map projectile-mode-map
;;          ("C-c p" . projectile-command-map))
;;   :config
;;   (dolist (dir '("^node_modules$"))
;;     (add-to-list 'projectile-globally-ignored-directories dir))
;;   :custom
;;   (projectile-use-git-grep t)
;;   (projectile-indexing-method 'alien)
;;   (projectile-kill-buffers-filter 'kill-only-files)
;;   ;; Ignore uninteresting files. It has no effect when using alien mode.
;;   (projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
;;   (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so" ".a"))
;;   (projectile-ignored-projects `("~/"
;;                                  "/tmp/"
;;                                  "/private/tmp/"
;;                                  ,package-user-dir)))
;; (use-package project-x
;;   :ensure nil
;;   :init (me/vc-install :fetcher "github" :repo "karthink/project-x")
;;   :after project
;;   :config
;;   (setq project-x-save-interval 600)    ;Save project state every 10 min
;;   (project-x-mode 1))

(use-package magit)

(provide 'init-project)
;;; init-project.el ends here
