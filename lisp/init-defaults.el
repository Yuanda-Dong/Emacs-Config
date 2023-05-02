;;; init-defaults.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Use UTF-8 everywhere
(set-language-environment "UTF-8")
(set-locale-environment "en_AU.UTF-8")

;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

(setq user-full-name       "Yuanda Dong"
      user-real-login-name "Yuanda Dong"
      user-login-name      "me"
      user-mail-address    "dongyuanda@gmail.com")

;; enable useful functions
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Inhibit the default startup screen
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(setq initial-major-mode 'org-mode)
(defun display-startup-echo-area-message ()
  (message "Let the hacking begin!"))

;; Mouse Support
;; (setq dired-mouse-drag-files t)                   ; added in Emacs 29
;; (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;;Linux specific
(setq x-gtk-use-system-tooltips t
      x-gtk-use-native-input t
      x-underline-at-descent-line t)

;; Use system trash can
(setq delete-by-moving-to-trash t)

;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
;; for the key passphrase.
(setq epg-pinentry-mode 'loopback)

;; Optimize for very long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; stretching
(setq x-stretch-cursor t)

;; Always load the newest file
(setq load-prefer-newer t)

;; Cutting and pasting use primary/clipboard
;; (setq select-enable-primary t
;;       select-enable-clipboard t)

;; No gc for font caches
(setq inhibit-compacting-font-caches t)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(setq blink-cursor-mode nil)

;; Smooth Scrolling
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 8)
(customize-set-variable 'scroll-preserve-screen-position t)
(pixel-scroll-precision-mode)

;; The nano style for truncated long lines.
(setq auto-hscroll-mode 'current-line)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Dont move points out of eyes
(setq mouse-yank-at-point t)

;; readable defaults
(setq-default fill-column 80)

;; Treat camelCase words as subwords
(global-subword-mode 1)

;; No tabs
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))
(setq-default tab-width 4)

;; Sane defaults
(setq use-short-answers t)

;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
(setq y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

;; Do not truncate the results of =eval-expression=
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Always insert a final newline, as per the Unix convention.
(setq require-final-newline t)

;; Make scripts executable after save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Don't nag when trying to create a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; WTF is a hello file
(defalias #'view-hello-file #'ignore)

;; unset 2-window scrolling shortcuts
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "S-<f2>"))

;; Don't nag when following symlinks to files under version control
(setq vc-follow-symlinks t)

;; Disable warnings from obsolete advice system
(setq ad-redefinition-action 'accept)

;; Always focus on help windows
(setq help-window-select t)

;; Don't show 'press q to close' message
(advice-add 'help-window-display-message :override #'ignore)

;; Don't confirm before killing subprocesses on exit
(setq confirm-kill-processes nil)
(defun config--suppress-no-process-prompt (fn &rest args)
  (cl-labels ((process-list () nil))
    (apply fn args)))
(advice-add #'save-buffers-kill-emacs :around #'config--suppress-no-process-prompt)

;; Convert ANSI color codes to text properties in shell output
(autoload 'ansi-color-apply-on-region "ansi-color")

(defun config--display-ansi-codes (buf &rest _)
  (and (bufferp buf)
       (string= (buffer-name buf) "*Shell Command Output*")
       (with-current-buffer buf
         (ansi-color-apply-on-region (point-min) (point-max)))))

(advice-add #'display-message-or-buffer :before #'config--display-ansi-codes)

;; Large file support
(defconst config--large-file-allowed-extensions
  '("pdf" "png" "jpg" "jpeg"))

(defun config--dont-abort-if-allowed-extension (f &rest args)
  (-let [(_size _op filename) args]
    (unless (--any-p (f-ext-p filename it) config--large-file-allowed-extensions)
      (apply f args))))

(advice-add #'abort-if-file-too-large :around #'config--dont-abort-if-allowed-extension)


;; Save the minibuffer history across sessions
(use-package savehist
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables '(kill-ring
                                   compile-command
                                   search-ring
                                   regexp-search-ring
                                   vertico-repeat-history)))

;; Highlight parenthesises
;; (use-package paren
;;   :init (setq show-paren-delay 0)
;; (setq show-paren-when-point-inside-paren t)
;;   :hook (after-init . show-paren-mode)
;;   :custom
;;   (show-paren-when-point-in-periphery t))

;; (electric-pair-mode)
;; (show-paren-mode nil)

;; Show line/column number and more
(use-package simple
  :ensure nil
  :custom
  ;; show line/column/filesize in modeline
  (line-number-mode t)
  (column-number-mode t)
  ;; (size-indication-mode t)
  ;; No visual feedback on copy/delete.
  (copy-region-blink-delay 0)
  (delete-pair-blink-delay 0)
  ;; confusing if no fringes (GUI only).
  ;; (visual-line-fringe-indicators '(nil right-curly-arrow))
  ;; don't save current clipboard text before replacing it
  (save-interprogram-paste-before-kill nil)
  ;; eliminate duplicates
  (kill-do-not-save-duplicates t)
  ;; include '\n' when point starts at the beginning-of-line
  (kill-whole-line t)
  ;; show cwd when `shell-command' and `async-shell-command'
  (shell-command-prompt-show-cwd t)
  ;; show the name of character in `what-cursor-position'
  (what-cursor-show-names t)
  ;; List only applicable commands.
  ;;
  ;; ``` elisp
  ;; (defun foo ()

  ;;   (interactive nil org-mode)
  ;;   (message "foo"))
  ;; ```
  ;;
  ;; M-x foo should only be available in `org-mode` or modes derived from `org-mode`.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Type text
(use-package text-mode
  :ensure nil
  :custom
  ;; better word wrapping for CJK characters
  (word-wrap-by-category t)
  ;; paragraphs
  (sentence-end-double-space nil))

;; Back to the previous position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Enable `repeat-mode' to reduce key sequence length
;;
;; If we have been idle for `repeat-exit-timeout' seconds, exit the repeated
;; state.
;; (use-package repeat
;;   :custom
;;   (repeat-mode t)
;;   (repeat-exit-timeout 3)
;;   (repeat-exit-key (kbd "RET")))

;; Workaround with minified source files
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Revert buffers automatically if the file changes on disk
(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  :config
  (setq global-auto-revert-non-file-buffers t)
  (setq-default auto-revert-interval 2)
  (auto-revert-set-timer)
  )

;; Turns URLs and mailto links into clickable buttons
(use-package goto-addr
  :hook (prog-mode . goto-address-prog-mode))

(use-package auth-source
  :custom
  auth-sources '("~/.authinfo.gpg"))

(defun me/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
        nil)))

;; Notifications
;; Actually, `notify-send' is not defined in notifications package, but the
;; autoload cookie will make Emacs load `notifications' first, then our
;; `defalias' will be evaluated.
(pcase system-type
  ('gnu/linux
   (use-package notifications
     :commands notify-send
     :config
     (defalias 'notify-send 'notifications-notify)))
  ('darwin
   (defun notify-send (&rest params)
     "Send notifications via `terminal-notifier'."
     (let ((title (plist-get params :title))
           (body (plist-get params :body)))
       (start-process "terminal-notifier"
                      nil
                      "terminal-notifier"
                      "-group" "Emacs"
                      "-title" title
                      "-message" body
                      "-activate" "org.gnu.Emacs"))))
  (_
   (defalias 'notify-send 'ignore)))

;; Typed text replaces the selection if the selection is active, pressing delete or backspace deletes the selection.
;; (delete-selection-mode)

;; Prevent cursor from entering the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Show additional information in prompt if completing-read-multiple is used
(use-package crm
  :config
  (define-advice completing-read-multiple (:filter-args (args) show-indicator)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args))))


;; Cache passwords
(use-package password-cache
  :custom
  (password-cache t)
  ;; (password-cache-expiry 300))
  (password-cache-expiry (* 3600 24)))

(use-package debug
  :ensure nil
  :config
  (defun config-debug--ad-eval-expr-in-frame (f exp)
    (interactive (list (read (eval-expression-interactively--read "Eval (in frame): "))))
    (funcall f exp))

  (advice-add 'debugger-eval-expression
              :around #'config-debug--ad-eval-expr-in-frame
              '((name . use-eval-expression-interactively--read)))
  (advice-add 'debugger-record-expression
              :around #'config-debug--ad-eval-expr-in-frame
              '((name . use-eval-expression-interactively--read))))


;; Better eval-expression
;; Define an alternative version of =eval-expression= that uses =emacs-lisp-mode= to provide font-locking, and handles =smartparens= better.
;; See: [[https://lists.gnu.org/archive/html/help-gnu-emacs/2014-07/msg00135.html][Re: How properly utilize the minibuffer and inactive minibuffer startup]]

(defvar eval-expression-interactively-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map (kbd "<escape>") #'abort-minibuffers)
    (define-key map (kbd "C-g") #'abort-minibuffers)
    map))

(defun eval-expression-interactively--read (prompt &optional initial-contents)
  (let ((minibuffer-completing-symbol t))
    (minibuffer-with-setup-hook
        (lambda ()
          (let ((inhibit-message t))
            (emacs-lisp-mode)
            (use-local-map eval-expression-interactively-map)
            (setq font-lock-mode t)
            (funcall font-lock-function 1)))
      (read-from-minibuffer prompt initial-contents
                            eval-expression-interactively-map nil
                            'read-expression-history))))

(autoload 'pp-display-expression "pp")
(autoload 'pp-to-string "pp")

(defun eval-expression-interactively (expression &optional arg)
  "Like `eval-expression' with nicer input handling.

  - Use `emacs-lisp-mode' to provide font locking and better
    integration with other packages.

  - Use the `pp' library to display the output in a readable form.

  EXPRESSION is a Lisp form to evaluate.

  With optional prefix ARG, insert the results into the buffer at
  point."
  (interactive (list (read (eval-expression-interactively--read "Eval: "))
                     current-prefix-arg))
  (condition-case _
      (if arg
          (insert (pp-to-string (eval expression lexical-binding)))
        (pp-display-expression (eval expression lexical-binding)
                               "*Pp Eval Output*" t))
    (error
     (eval-expression expression arg))))

;; Bind this command to ~M-:~
(general-define-key :keymaps 'override :states '(normal motion visual)
                    "M-:" 'eval-expression-interactively)

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun switch-to-messages-buffer ()
  "Switch to Messages buffer."
  (interactive)
  (switch-to-buffer (messages-buffer)))

;; slience advices

(defun advice-ignore-errors (f &rest args)
  (ignore-errors
    (apply f args)))

(defun advice-inhibit-logging (f &rest args)
  (let ((message-log-max))
    (apply f args)))

(defun advice-clear-minibuffer (&rest _)
  (message ""))

(use-package simpleclip)
(add-to-list 'warning-suppress-types '(defvaralias))
(provide 'init-defaults)
;;; init-defaults.el ends here
