;;; init-edit.el -- User init file for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package undo-fu
  :demand t)

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(undo-fu-session-global-mode)


;; (setq display-line-numbers-type 'relative)
(setq display-line-numbers-width-start t)
;; (setq display-line-numbers-grow-only t)
;; (setq display-line-numbers-width 3)
(global-visual-line-mode)
;; (dolist (mode '(lsp-help-mode-hook helpful-mode-hook prog-mode-hook latex-mode-hook text-mode-hook conf-mode-hook))
;;   ;; (add-hook mode #'display-line-numbers-mode)
;;   (add-hook mode 'visual-line-mode)
;;   (add-hook mode 'hl-line-mode)
;; )
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Override some modes which derive from the above
;; (dolist (mode '(org-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (global-visual-line-mode)

;; (use-package hl-line
;;   :hook (after-init . global-hl-line-mode))
;; Server mode.
;; Use emacsclient to connect
(use-package server
  :ensure nil
  :hook (after-init . server-mode))
(setq server-client-instructions nil)
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :preface
  (defun config-evil--execute-Q-macro (count)
    "Execute the macro bound to the Q register.

  COUNT is the number of repetitions."
    (interactive (list
                  (if current-prefix-arg
                      (if (numberp current-prefix-arg) current-prefix-arg 0)
                    1)))
    (evil-execute-macro count (evil-get-register ?Q t)))
  :hook (after-init . evil-mode)
  ;; Don't quit Emacs on `:q'.
  ;;
  ;; Rebind `f'/`s' to mimic `evil-snipe'.
  :bind (([remap evil-quit] . kill-this-buffer)
         :map evil-motion-state-map
         ("f" . evil-avy-goto-char-in-line)
         :map evil-normal-state-map
         ("s" . evil-avy-goto-char-timer)
         :map evil-visual-state-map
         ("C-c" . simpleclip-copy)
         :map evil-insert-state-map
         ("C-v" . simpleclip-paste))
  :config
  (setq
   ;; evil-motion-state-cursor '("plum3" box)
   evil-visual-state-cursor '("gray" hbar)
   evil-normal-state-cursor '("IndianRed" box)
  ;;  evil-insert-state-cursor '("chartreuse3" bar)
   ;;  evil-emacs-state-cursor  '("SkyBlue2" (box . t))
   )


  ;; (evil-set-initial-state 'anaconda-mode-view-mode 'motion)
  (evil-set-initial-state 'diff-mode 'motion)
  ;; (evil-set-initial-state 'ert-simple-view-mode 'motion)
  ;; (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'grep-mode 'normal)
  ;; (evil-set-initial-state 'haskell-debug-mode 'motion)
  (evil-set-initial-state 'helpful-mode 'motion)
  (evil-set-initial-state 'ibuffer-mode 'motion)
  (evil-set-initial-state 'message-mode 'motion)
  ;; (evil-set-initial-state 'nix-repl-mode 'insert)
  ;; (evil-set-initial-state 'occur-mode 'normal)
  ;; (evil-set-initial-state 'org-agenda-mode 'motion)
  ;; (evil-set-initial-state 'prodigy-mode 'motion)
  ;; (evil-set-initial-state 'profiler-report-mode 'motion)
  ;; (evil-set-initial-state 'racer-help-mode 'motion)
  ;; (evil-set-initial-state 'tabulated-list-mode 'motion)
  ;; (evil-set-initial-state 'vterm-mode 'emacs)
  ;; (evil-set-initial-state 'wdired-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'motion)

  :custom
  (evil-shift-width 2)
  (evil-mode-line-format nil)
  (evil-undo-system 'undo-fu)
  ;; Switch to the new window after splitting
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  ;; when `visual-line-mode' enabled, exchange j/k with gj/gk
  (evil-echo-state nil)
  (evil-respect-visual-line-mode t)
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  ;; (evil-want-fine-undo t)
;;  (evil-want-C-g-bindings t)
  (evil-want-C-i-jump nil)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-kill-on-visual-paste nil)
  (evil-symbol-word-search t)
  ;; (evil-want-o/O-to-continue-comments t)
  :config

  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  :general
  (:states 'normal "Q" #'config-evil--execute-Q-macro))

;; Teach ~<~ and ~>~ to shift text in a context-sensitive way
(use-package evil
  :general (:states 'visual
                    "<" #'config-evil--shift-left
                    ">" #'config-evil--shift-right)
  :preface
  (defun config-evil--shift-left (&optional beg end)
    "Shift left, keeping the region active.

  BEG and END are the bounds of the active region."
    (interactive "r")
    (evil-shift-left beg end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun config-evil--shift-right (&optional beg end)
    "Shift right, keeping the region active.

  BEG and END are the bounds of the active region."
    (interactive "r")
    (evil-shift-right beg end)
    (evil-normal-state)
    (evil-visual-restore)))

(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(advice-add 'evil-yank :around 'meain/evil-yank-advice)

(use-package evil-mc
  :after evil
  :config
  ;; (evil-define-key 'visual evil-mc-key-map
  ;;   "A" #'evil-mc-make-cursor-in-visual-selection-end
  ;;   "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  (evil-define-local-var evil-mc-custom-paused nil
    "Paused functionality when there are multiple cursors active.")

  (defun evil-mc-pause-smartchr-for-mode (mode)
    "Temporarily disables the smartchr keys for MODE."
    (let ((m-mode (if (atom mode) mode (car mode)))
          (s-mode (if (atom mode) mode (cdr mode))))
      (let ((init (intern (concat "smartchr/init-" (symbol-name s-mode))))
            (undo (intern (concat "smartchr/undo-" (symbol-name s-mode)))))
        (when (eq major-mode m-mode)
          (funcall undo)
          (push `(lambda () (,init)) evil-mc-custom-paused)))))

  (defun evil-mc-before-cursors-setup-hook ()
    "Hook to run before any cursor is created.
Can be used to temporarily disable any functionality that doesn't
play well with `evil-mc'."
    (mapc 'evil-mc-pause-smartchr-for-mode
          '(web-mode js2-mode java-mode (enh-ruby-mode . ruby-mode) css-mode))
    (when (boundp 'whitespace-cleanup-disabled)
      (setq whitespace-cleanup-disabled t)
      (push (lambda () (setq whitespace-cleanup-disabled nil)) evil-mc-custom-paused)))

  (defun evil-mc-after-cursors-teardown-hook ()
    "Hook to run after all cursors are deleted."
    (dolist (fn evil-mc-custom-paused) (funcall fn))
    (setq evil-mc-custom-paused nil))

  (add-hook 'evil-mc-before-cursors-created 'evil-mc-before-cursors-setup-hook)
  (add-hook 'evil-mc-after-cursors-deleted 'evil-mc-after-cursors-teardown-hook)

  (defvar evil-mc-mode-line-prefix "\u24dc"
    "Override of the default mode line string for `evil-mc-mode'.")

  (global-evil-mc-mode)
  ;; :bind (:map evil-normal-state-map)
  ;; ("m" . 'evil-mc-key-map)
  )

;; (eval-after-load 'evil-define-key
;;   )

;; =evil-org= - Improve Evil integration with org-mode
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-collection
  ;; :ensure nil
  :vc (:fetcher github :repo meliache/evil-collection)
  :after evil
  ;; :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :after (evil)
  :demand t
  :config (global-evil-surround-mode +1)
  :general
  (:states 'visual :keymaps 'evil-surround-mode-map
           "S" #'evil-surround-region)
           ;; "S" #'evil-substitute)
  :custom
  (evil-surround-pairs-alist '((?\( . ("(" . ")"))
                               (?\[ . ("[" . "]"))
                               (?\{ . ("{" . "}"))

                               (?\) . ("(" . ")"))
                               (?\] . ("[" . "]"))
                               (?\} . ("{" . "}"))

                               (?# . ("#{" . "}"))
                               (?b . ("(" . ")"))
                               (?B . ("{" . "}"))
                               (?> . ("<" . ">"))
                               (?t . evil-surround-read-tag)
                               (?< . evil-surround-read-tag)
                               (?f . evil-surround-function))))
;;Fix Magit
;; (use-package evil-surround
;;   :preface
;;   (defun config-evil-surround-inhibit-in-magit-section-mode (&rest args)
;;     (if (derived-mode-p 'magit-section-mode)
;;         (list -1)
;;       args))
;;   :config
;;   (advice-add 'evil-surround-mode :filter-args #'config-evil-surround-inhibit-in-magit-section-mode))


;; =evil-matchit= - Teach ~%~ how to match more kinds of pairs
(use-package evil-matchit
  :after (evil)
  :demand t
  :config
  (global-evil-matchit-mode +1))

;;Show search counts
(use-package evil-anzu
  :hook (after-init . global-anzu-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))
  ;; (text-mode . rainbow-delimiters-mode))

(use-package smartparens
  ;; :hook
  ;; (prog-mode . smartparens-mode)
  ;; (text-mode . smartparens-mode)
  ;; :demand t
  ;; :general
  ;; (:keymaps 'smartparens-strict-mode-map
  ;;  [remap c-electric-backspace] #'sp-backward-delete-char)
  ;; (:states 'insert
  ;;  ")" #'sp-up-sexp)
  ;; (:states 'normal
  ;;  "D" #'sp-kill-hybrid-sexp)
  ;; :custom
  ;; (sp-show-pair-delay 0.2)
  ;; (sp-show-pair-from-inside t)
  ;; (sp-cancel-autoskip-on-backward-movement nil)
  ;; (sp-highlight-pair-overlay nil)
  ;; (sp-highlight-wrap-overlay nil)
  ;; (sp-highlight-wrap-tag-overlay nil)
  ;; (sp-navigate-close-if-unbalanced t)
  ;; (sp-message-width nil)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

(show-paren-mode -1)
;; =hideshow= - Basic code folding
;; (use-package hideshow
;;   :hook (prog-mode . hs-minor-mode)
;;   :config
;;   (advice-add 'hs-hide-all :around #'advice-ignore-errors)
;;   (advice-add 'hs-hide-block :around 'advice-ignore-errors)
;;   (advice-add 'hs-minor-mode :around #'advice-ignore-errors)
;;   (advice-add 'hs-show-all :around #'advice-ignore-errors)
;;   (advice-add 'hs-show-block :around #'advice-ignore-errors)
;;   (advice-add 'hs-toggle-hiding :around #'advice-ignore-errors))

(use-package hideshow
  ;; :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                                          hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  :custom
  (hs-set-up-overlay #'hideshow-folded-overlay-fn))

(use-package wgrep
  :commands (wgrep-change-to-wgrep-mode)
  :bind (:map grep-mode-map
              ("C-z C-z" . wgrep-finish-edit)))

;; Avy
(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

;;Improve basic editing configuration for all modes

;; Use control key to transpose lines up and down
(defun transpose-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun transpose-line-down ()
  "Move the current line up."
  (interactive)

  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Useful interactive functions
(defun insert-uuid (&optional upcase-p)
  "Insert a UUID at point.

  Prefix arg UPCASE-P determines whether the UUID is downcased or
  upcased on insertion."
  (interactive "*P")
  (let ((uuid (string-trim (shell-command-to-string "uuidgen"))))
    (insert (if upcase-p (upcase uuid) (downcase uuid)))))

(defun insert-date (str)
  "Read date string STR interactively and insert it at point."
  (interactive (list
                (if (not current-prefix-arg)
                    (format-time-string "%F")
                  (let ((formats (seq-map #'format-time-string
                                          '("%F"
                                            "%F %R"
                                            "%X"
                                            "%c"))))
                    (completing-read "Format: " formats nil t)))))
  (insert str))


(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive "*")
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; =ws-butler= - Automatic whitespace cleanup while editing
(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))

;; =unfill= - Paragraph fill/unfill
;; =unfill= provides a command that is the opposite of fill-paragraph.
(use-package unfill
  :commands (unfill-region unfill-paragraph unfill-toggle))

;; =align= - Provides useful functions for aligning text
(use-package align)

;; =hide-comnt= - Toggle whether comments are visible
;; (use-package hide-comnt
;;   :commands (hide/show-comments-toggle))

;; =dumb-jump= - Generic jump-to-definition support
;; =dump-jump= provides a good fallback for navigating to definitions in the absence
;; of an LSP or semantic analysis.

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; =ispell= - Spellchecking commands
(use-package ispell
  :commands (ispell-check-version ispell-find-aspell-dictionaries)

  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "en_GB")
  (ispell-silently-savep t)

  :config
  (ispell-check-version)
  (setq ispell-dictionary-alist (ispell-find-aspell-dictionaries)))

;; =flyspell= - Incremental spellchecking
(use-package flyspell
  ;; :hook
  ;; (git-commit-mode . flyspell-mode)
  ;; (markdown-mode . flyspell-mode)
  ;; (org-mode . flyspell-mode)
  :custom
  (flyspell-issue-welcome-flag nil)
  (flyspell-default-dictionary "en_GB"))

;; Prevent =flyspell= from showing suggestions in more contexts
(use-package flyspell
  :after (org)
  :config
  (defun flyspell-on-org-verify (result)
    (and result
         (not (seq-intersection (face-at-point nil t)
                                '(org-link verb-header)))))
  (advice-add 'org-mode-flyspell-verify :filter-return #'flyspell-on-org-verify))


;; Editor config
(use-package editorconfig
  :hook (after-init . editorconfig-mode)
  :init
  (define-advice editorconfig--advice-insert-file-contents (:around (fn &rest args) handle-errors)
    (condition-case err
        (apply fn args)
      (file-missing
       nil)
      (error
       (throw (car err) (cdr err))))))

;; ** =envrc= - Support direnv files :Disabled:
;; #+begin_src emacs-lisp
;; (use-package envrc
;;   :hook (after-init . envrc-global-mode))
;; #+end_src
(use-package  inheritenv
)

;; ** =rainbow-mode= - Apply colours to hex strings in buffers
(use-package rainbow-mode
  :hook
  (help-mode . rainbow-mode)
  (emacs-lisp-mode . rainbow-mode)
  (css-mode . rainbow-mode))

;; =string-inflection= - Command to change string case at point
(use-package string-inflection)

;; Emacs’ default commenting system is nice, but I don’t find it smart enough for me.
(use-package evil-nerd-commenter
  :demand t
  :after evil
)


;; (defun increment-number-at-point ()
;;   (interactive)
;;   (skip-chars-backward "0-9")
;;   (or (looking-at "[0-9]+")
;;       (error "No number at point"))
;;   (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; (global-set-key (kbd "C-c +") 'increment-number-at-point)
(provide 'init-edit)
;;; init-edit.el ends here
