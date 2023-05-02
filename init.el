;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Set background placeholder
(set-background-color  "#282c34" )
(set-foreground-color  "#bbc2cf" )

;; Reduce fanfare
;; TODO See https://yrh.dev/blog/rant-obfuscation-in-emacs/
(setq inhibit-startup-echo-area-message "Yuanda")

;; (require 'cl-lib)
;; (cl-defun me/vc-install (&key (fetcher "github") repo name rev backend)
;;   "Install a package from a remote if it's not already installed.
;; This is a thin wrapper around `package-vc-install' in order to
;; make non-interactive usage more ergonomic.  Takes the following
;; named arguments:

;; - FETCHER the remote where to get the package (e.g., \"gitlab\").
;;   If omitted, this defaults to \"github\".

;; - REPO should be the name of the repository (e.g.,
;;   \"slotThe/arXiv-citation\".

;; - NAME, REV, and BACKEND are as in `package-vc-install' (which
;;   see)."
;;   (let* ((url (format "https://www.%s.com/%s" fetcher repo))
;;          (iname (when name (intern name)))
;;          (pac-name (or iname (intern (file-name-base repo)))))
;;     (unless (package-installed-p pac-name)
;;       (package-vc-install url iname rev backend))))


(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Native compile
(setq package-native-compile t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer nil)
  (setq use_package_always_demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil))

(eval-when-compile
  (require 'use-package))
(setq byte-compile-warnings '((not cl-functions)))
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; enable debugging for duration of startup sequence
(unless noninteractive
  (setq debug-on-error t))

(add-hook 'after-init-hook
          (lambda ()
            (setq debug-on-error nil))
          98)

;; GC optimization
(use-package gcmh
  :demand t
  :config (gcmh-mode))

;; Tune LSP performance
(setq read-process-output-max (* 3 1024 1024))

;; Module load path
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))

;; keep clean
(use-package no-littering
  :demand t
  :config
  (setq user-emacs-directory (expand-file-name "~/.cache/emacs")
        url-history-file (expand-file-name "url/history" user-emacs-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
  (when (file-exists-p custom-file) ; Don’t forget to load it, we still need it
    (load custom-file))
)

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  :custom
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-max-saved-items 100))


;;Common advice
(defun advice-ignore-errors (f &rest args)
  (ignore-errors
    (apply f args)))

(defun advice-inhibit-logging (f &rest args)
  (let ((message-log-max))
    (apply f args)))

(defun advice-clear-minibuffer (&rest _)
  (message ""))

(use-package general
  :demand t
  :init
  (general-auto-unbind-keys)
)
(require 'init-defaults)
(require 'init-project)
(require 'init-ui)
(require 'init-edit)
(require 'init-completion)
(require 'init-code)
(require 'init-shell)
(require 'init-manager)
(require 'init-writer)
(require 'init-pub)
(require 'init-apps)
(require 'init-keybinds)
(require 'init-hydra)
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-vc-selected-packages
   '((vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package")))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
