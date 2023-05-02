;;; init-completion.el -- User init file for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; =orderless= - order-insensitive matching algorithm
(use-package orderless
  :demand t
  :config

  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; You may want to combine the `orderless` style with `substring` and/or `basic`.
  ;; There are many details to consider, but the following configurations all work well.
  ;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
  ;; special styles for special completion categories, e.g., partial-completion for files.
  ;;
  ;; 1. (setq completion-styles '(orderless))
  ;; This configuration results in a very coherent completion experience,
  ;; since orderless is used always and exclusively. But it may not work
  ;; in all scenarios. Prefix expansion with TAB is not possible.
  ;;
  ;; 2. (setq completion-styles '(substring orderless))
  ;; By trying substring before orderless, TAB expansion is possible.
  ;; The downside is that you can observe the switch from substring to orderless
  ;; during completion, less coherent.
  ;;
  ;; 3. (setq completion-styles '(orderless basic))
  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  ;;
  ;; 4. (setq completion-styles '(substring orderless basic))
  ;; Combine substring, orderless and basic.
  ;;
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-affix-dispatch))
  :custom (completions-detailed t)
  )

;; =vertico= - Completion & narrowing for minibuffer prompts

(use-package vertico
  :hook ((after-init . vertico-mode)
   (minibuffer-setup . vertico-repeat-save)) ; Make sure vertico state is saved for `vertico-repeat'

  :general (:keymaps 'vertico-map
                     "C-<return>" 'vertico-exit-input
                     "M-<return>" 'minibuffer-force-complete-and-exit
                     "C-j" 'vertico-next
                     "C-k" 'vertico-previous
                     ;; Toggle Vertico multiforms in active minibuffer
               ;;    "C-i" #'vertico-quick-insert
                ;;   "C-o" #'vertico-quick-exit
                    "M-G" #'vertico-multiform-grid
                     "M-F" #'vertico-multiform-flat
                     "M-R" #'vertico-multiform-reverse
                     "M-U" #'vertico-multiform-unobtrusive
                      "<tab>" #'vertico-insert) ; Set manually otherwise setting `vertico-quick-insert' overrides this
                     ;; "TAB" #'vertico-insert)

                       (:states '(normal insert visual motion)
            "M-." #'vertico-repeat) ; Perfectly return to the state of the last Vertico minibuffer usage
  :config
  (vertico-multiform-mode)
  ;; tweaking vertico displays
(advice-add #'vertico--format-candidate :around
                                        (lambda (orig cand prefix suffix index _start)
                                          (setq cand (funcall orig cand prefix suffix index _start))
                                          (concat
                                           (if (= vertico--index index)
                                               (propertize "» " 'face 'vertico-current)
                                             "  ")
                                           cand)))
  :custom
  (vertico-multiform-categories
   '(
    (consult-grep buffer)
    ;;  (consult-location buffer)
    ;;  (imenu buffer)
     (org-roam-node indexed))
   )
  (vertico-cycle t)

  ;; (vertico-resize t)
  )

;; Make completion case-insensitive
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Fix errors raised in =vertico--exhibit=
  (advice-add 'vertico--exhibit :around #'advice-ignore-errors)

;; Improve completion UX for files & directories
;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; * =all the icons= completion

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; =embark= - adds a general contextual menu
  (use-package embark
    :demand t
    :init
  (setq prefix-help-command #'embark-prefix-help-command)
    :general ("C-@" 'embark-act)
    :config
    (setq embark-indicators '(embark--vertico-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
    :custom
    embark-prompter 'embark-completing-read-prompter
)

  (use-package embark-consult
    :after (:all embark consult)
    :demand t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

;; =consult= Consulting completing-read
(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :demand t
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.

  :bind ("C-r" . 'consult-history)
  :config
  (pcase-let ((`("rg" . ,args) (s-split " " consult-ripgrep-args t)))
    (unless (seq-contains-p args "--follow")
      (setq consult-ripgrep-args (concat "rg " (s-join " " (cons "--follow" args))))))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)
   consult-buffer :preview-key "C-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)

  )


;; Consult-flyspell
(use-package consult-flyspell)
;; * =dogears= - remember where you've been
  ;; (use-package dogears
  ;;   :hook (after-init . dogears-mode))

;; * =corfu= - completion UI
(use-package corfu
  :demand t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  :config
 ;; (corfu-popupinfo-mode 1)
  (eldoc-add-command #'corfu-insert)

  :custom
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-auto nil)
  (corfu-on-exact-match nil)
  ;; (corfu-quit-no-match 'separator)
  ;; (corfu-preselect-first nil)
    (corfu-preselect 'first)      ;; Preselect the prompt
(corfu-max-candidates 50)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
 (corfu-popupinfo-delay 0)
;; (corfu-quit-at-boundary t )    ;; Automatically quit at word boundary
    (corfu-quit-no-match t ))       ;; Automatically quit if there is no match

(use-package emacs
   :init
   (setq completion-cycle-threshold 3)
;; Perform both indentation & text completion with TAB.
   (setq tab-always-indent 'complete))



;; Enable =corfu= for eval-expression, etc
(use-package corfu
  :init
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  :hook (minibuffer-setup . corfu-enable-in-minibuffer))

;; Exiting =corfu= also returns to evil normal state
;; (use-package corfu
;;   :init
;;   (defun config-corfu-quit-and-enter-normal-state ()
;;     (interactive)
;;     (corfu-quit)
;;     (when (evil-insert-state-p)
;;       (evil-normal-state)))
;;   :general
;;   (:keymaps 'corfu-map
;;    [remap corfu-quit] 'config-corfu-quit-and-enter-normal-state))

;; Use a less aggressive completion configuration in =eshell=
;; (use-package eshell
;;   :after corfu
;;   :demand t
;;   :init
;;   (defun config-corfu-eshell-setup ()
;;     (setq-local corfu-auto nil)
;;     (corfu-mode +1))
;;   :hook (eshell-mode . config-corfu-eshell-setup))

;; =dabbrev= - configure integration with =corfu=
  (use-package dabbrev
    :custom
    (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


;; =kind-icon= - Show icons in =corfu= completion popups
(use-package kind-icon
  :after corfu
  :demand t
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
  (add-hook 'after-load-theme-functions (lambda (_)
                                          (kind-icon-reset-cache))))

;; =cape= - extensions to built-in completion mechanisms
  (use-package cape
;;    :general ("M-/" 'completion-at-point)
    :demand t
    :preface
    (defun config-cape-default-setup ()
      (add-to-list 'completion-at-point-functions 'cape-file)
      (add-to-list 'completion-at-point-functions 'cape-dabbrev)
      (add-to-list 'completion-at-point-functions 'cape-tex)
      (add-to-list 'completion-at-point-functions 'cape-rfc1345)
      (add-to-list 'completion-at-point-functions 'cape-keyword))
    (defun config-cape-lisp-setup ()
      (make-local-variable 'completion-at-point-functions)
      (config-cape-default-setup)
      (add-to-list 'completion-at-point-functions 'cape-symbol))
    ;; (defun config-cape-text-mode-setup ()
    ;;   (make-local-variable 'completion-at-point-functions)
    ;;   (config-cape-default-setup)
    ;;   (add-to-list 'completion-at-point-functions 'cape-ispell))
    :hook
    (emacs-lisp-mode . config-cape-lisp-setup)
    (lisp-data-mode . config-cape-lisp-setup)
    (ielm-mode . config-cape-lisp-setup)
    ;; (text-mode . config-cape-text-mode-setup)
    (yaml-mode . config-cape-default-setup)
    :init
    (config-cape-default-setup))

(provide 'init-completion)
;;; init-completion.el ends here
