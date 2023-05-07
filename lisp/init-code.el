;; init-code.el -- User init file for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;; (add-to-list 'major-mode-remap-alist
;;              '(c-or-c++-mode . c-or-c++-ts-mode))
(use-package leetcode)


(defun my/leetcode-view-problem ()
  "Read an integer from user input and print it."
  (interactive)
  (let ((num (read-number "Enter question id: ")))
    (leetcode-view-problem num)
    (shell-command (format "leetup pick -l rust %d" num))
    ))
;; (add-to-list 'load-path
;;               "~/.emacs.d/plugins/yasnippet")

;; (cl-deftype json-plist () '(satisfies json-plist-p))

;; (let ((org-confirm-babel-evaluate nil)
;;       (message-log-max nil)
;;       (inhibit-message t)
;; (use-package jupyter
;;   )
;; )



;; (advice-add 'yas-global-mode :around #'advice-inhibit-logging)
;; (advice-add 'yas-global-mode :after #'advice-clear-minibuffer)
;; (advice-add 'yas-global-mode :around #'advice-inhibit-logging)
;; (advice-add 'yas-global-mode :after #'advice-clear-minibuffer)
;; (use-package yasnippet
;;   :init (yas-global-mode))

(use-package yasnippet
  :ensure t
  :custom (yas-keymap-disable-hook
           (lambda () (and (frame-live-p corfu--frame)
                           (frame-visible-p corfu--frame))))
  :hook ((lsp-mode . yas-minor-mode)))

(use-package lsp-mode
  ;; :defer t
  ;; :ensure nil
  ;; :vc (:fetcher github :repo asandroq/lsp-mode)

  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (me/vc-install :fetcher "github" :repo "emacs-lsp/lsp-mode")
  (setq lsp-keymap-prefix "C-c l")

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless

  ;; (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (yaml-ts-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         (lsp-mode . sideline-mode)
         )
  :commands lsp
  :custom
  (lsp-use-plist t)
  (lsp-completion-provider :none)
  ;; (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.1)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-modeline-code-actions-segments '(count name))
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-inlay-hint-enable t)
  ;; (lsp-modeline-code-actions-enable nil)

  ;; (lsp-eldoc-enable-hover nil)
  ;; (lsp-signature-auto-activate nil)
)

;; Consult-lsp
(use-package consult-lsp
  :after lsp)

;; (add-hook 'lsp-after-open-hook (lambda ()
;;                                  (when (lsp-find-workspace 'rust-analyzer nil)
;;                                    (lsp-rust-analyzer-inlay-hints-mode))))
(use-package eldoc
  :custom
  (eldoc-idle-delay 0.1))

;; Lint tool
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay 0.25)
  (flycheck-indication-mode 'right-fringe))

(use-package lsp-ui
  :init (setq lsp-ui-sideline-enable nil)
  :hook (lsp-mode . lsp-ui-mode)
  ;; :custom
    ;; (lsp-ui-doc-position 'at-point)
  :custom
  (lsp-signature-auto-activate nil)
  (lsp-ui-sideline-ignore-duplicate t)
  ;; (lsp-ui-sideline-show-code-actions t)
  )

;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode))


;; (use-package treesit
;;   :defer t
;;   :ensure nil
;;   ;; :straight (:type built-in)
;;   :hook ((bash-ts-mode c-ts-mode c++-ts-mode
;;           html-ts-mode js-ts-mode typescript-ts-mode
;;           json-ts-mode rust-ts-mode tsx-ts-mode python-ts-mode
;;           css-ts-mode yaml-ts-mode) . lsp-deferred))
(use-package treesit
  :ensure nil
  :mode ("\\.yaml\\'" . yaml-mode)
  :commands (treesit-install-language-grammar)
  :custom
  (major-mode-remap-alist
   '((sh-mode         . bash-ts-mode)
     (c-mode          . c-ts-mode)
     (c++-mode        . c++-ts-mode)
     (csharp-mode     . csharp-ts-mode)
     (cmake-mode      . cmake-ts-mode)
     (css-mode        . css-ts-mode)
     (dockerfile-mode . dockerfile-ts-mode)
     (go-mode         . go-ts-mode)
     (java-mode       . java-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode    . json-ts-mode)
     (python-mode     . python-ts-mode)
     (ruby-mode       . ruby-ts-mode)
     (rust-mode       . rust-ts-mode)
     (rust-ts-mode    . rustic-mode)
     (conf-toml-mode  . toml-ts-mode)
     (rjsx-mode       . tsx-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-mode       . yaml-ts-mode))))

;; xref
(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))
;; Langs
;; C++
(setq c-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-style 'linux)
(setq c-basic-offset 4)
;; Python
(use-package lsp-pyright)
  ;; :hook (python-mode . (lambda ()
  ;;                         (require 'lsp-pyright)
  ;;                         (lsp-deferred))))  ; or lsp-deferred
;; Rust
;; (use-package rust-mode)

(use-package rustic
  ;; :init
  ;; (setq rustic-treesitter-derive t)
  ;; :vc (:fetcher github :repo yuuyins/rustic)
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp-deferred)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-indent-where-clause t)
  )



;; (add-hook 'lsp-after-open-hook (lambda ()
;;                                  (when (lsp-find-workspace 'rust-analyzer nil)
;;                                    (lsp-inlay-hints-mode))))
;; ISPC
(use-package ispc-mode)

(use-package lsp-treemacs
 ;;  :config
 ;; (lsp-treemacs-sync-mode 1)
)

;; Javascript
(setq js-indent-level 2)

;; cuda
(use-package cuda-mode)
(add-hook 'cuda-mode-hook
          (lambda ()
            ( setq c-basic-offset              4
                   flycheck-cuda-include-path (list "."))
            ))
;; (use-package c++-ts-mode
;;   :ensure nil
;;   :custom
;;   (c-ts-mode-indent-offset 4)
;;   (c-ts-mode-indent-style 'linux)
;;   :config
;;   (general-def c++-ts-mode-map ":" nil)
;; )

;; (use-package cc-mode
  ;; :hook (c-mode-common . cc-mode-setup)
  ;; :custom
  ;; (c-basic-offset 4)
  ;; (c-default-style "linux")
  ;; :config
  ;; (define-key c++-mode-map ":" nil)
  ;; (define-key c++-ts-mode-map ":" nil)
  ;; (defun cc-mode-setup ()
  ;;   (c-set-offset 'case-label '+)
  ;;   (setq-local comment-start "//"
  ;;               comment-end ""
  ;;               tab-width 4))
  ;; )

(use-package sideline
  :hook (flycheck-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flycheck)
        sideline-display-backend-name t      ; display the backend name
        ))

(use-package sideline-flycheck
  ;; :ensure nil
  ;; :init
  :vc (:fetcher github :repo emacs-sideline/sideline-flycheck)
  :hook (flycheck-mode . sideline-flycheck-setup))




(provide 'init-code)
;;; init-code.el ends here
