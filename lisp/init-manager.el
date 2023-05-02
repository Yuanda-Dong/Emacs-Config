;;; init-manager.el -- User init file for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :hook
  (dired-mode . (lambda ()
                  (visual-line-mode -1)
                  (setq-local truncate-partial-width-windows 100)))
  (dirvish-directory-view-mode . (lambda () (visual-line-mode -1)))
  :custom
  (dired-recursive-deletes 'always)
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("a" "~/Videos/AutoBangumi/Downloads/" "Anime")
     ("s" "~/Study/" "Study")
     ("d" "~/Downloads/"                "Downloads")
     ("v" "~/Videos/"                "Videos")
     ("D" "~/Documents/" "Documents")
     ("p" "~/Pictures/" "Pictures")
     ("u" "~/Uni/" "Uni")
     ("c" "~/config/" "config")
     ("n" "~/RoamNotes/" "RoamNotes")
     ("w" "~/Videos/Workout/" "Workout")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'

  (setq dirvish-open-with-programs
        `((,dirvish-audio-exts . ("mpv" "--profile=builtin-pseudo-gui" "%f"))
          (,dirvish-video-exts . ("mpv" "%f"))
          (("pdf") . ("zathura" "%f"))
          (("html") . ("firefox" "%f"))
          (("zip" "rar") . ("file-roller" "%f"))
          ))

  (setq mouse-1-click-follows-link nil)
  (define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") 'dired-mouse-find-file)

  (setq dirvish-header-line-height 30)
  (setq dirvish-mode-line-height 15) ; shorthand for '(25 . 25)

  ;; mode-line
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))

  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
       ;; "-l --almost-all --human-readable --group-directories-first --no-group")
        "-lv --human-readable --group-directories-first --no-group")

  (setq dirvish-preview-dispatchers
        (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers))
  ;; exa
  (dirvish-define-preview exa (file)
    "Use `exa' to generate directory preview."
    :require ("exa") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("exa" "-al" "--color=always" "--icons"
                 "--group-directories-first" ,file))))

  (add-to-list 'dirvish-preview-dispatchers 'exa)
  :custom-face
  (dired-directory ((t (:foreground "#51AFEF"))))
  ;; (dirvish-hl-line ((t (:background "#2a2e38"))))
  )

(use-package bufler
  :config
  (evil-set-initial-state 'bufler-list-mode 'motion)
  :commands (bufler bufler-list))

(winner-mode)

(use-package shackle
  :init
  (setq shackle-rules
        '((lsp-help-mode :align right :size 0.4 :noselect t)
          ("*ChatGPT*" :align right :size 0.4 :select t)
          ;; ("\\*Messages\\*" :align below :size 18 :select t)
          ;; (("^\\*vterm.*\\*$" vterm-mode) :select t :regexp t :align below :size 18)
          ;; (helpful-mode :align right :size 0.4 :select t)
          ;; (help-mode :align right :size 0.4 :select t)
          ;; (rustic-cargo-run-mode :align right :size 0.4)
          ;; ("\\*Messages\\*" :align below :size 18 :select t)
          )
        )
  :config
 (shackle-mode)
)

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  ;; (setq popper-group-function #'popper-group-by-project) ; project.el projects
  ;; (setq popper-group-function #'popper-group-by-directory) ; group by project.el
                                        ; project root, with
                                        ; fall back to
                                        ; default-directory
  (setq popper-window-height 18)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          ;; "\\*lsp-help\\*"
          ;; lsp-help-mode
          rustic-cargo-run-mode
          helpful-mode
          help-mode
          compilation-mode))
  ;; Match eshell, shell, term and/or vterm buffers
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                  "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                  "^\\*term.*\\*$"   term-mode   ;term as a popup
                  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                  )))

  :config
  ;; (setq popper-display-control 'user)
  (popper-mode +1)
  ;; :custom
  ;; (popper-mode-line nil)
)                ; For echo area hints
(use-package popper-echo
  :ensure nil
  :config
  (defun popper-message-shorten (name)
    (cond
     ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
      (concat (match-string 1 name)
              "(H)"))
     ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
      (concat (match-string 1 name)
              "(H)"))
     ((string-match "^\\*eshell:? ?\\(.*\\)\\*$" name)
      (concat (match-string 1 name)
              (if (string-empty-p (match-string 1 name)) "shell(E)" "(E)")))
     ((string-match "^\\*\\(.*?\\)\\(?:Output\\|Command\\)\\*$" name)
      (concat (match-string 1 name)
              "(O)"))
     ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
      (concat (match-string 1 name)
              "(L)"))
     ((string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
      (concat (match-string 1 name)
              "(C)"))
     (t name)))
  (setq popper-echo-transform-function #'popper-message-shorten)
  (setq popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  ;;       popper-echo-dispatch-actions t)
  (advice-add 'popper-echo :around
              (defun my/popper-echo-no-which-key (orig-fn)
                (let ((which-key-show-transient-maps nil))
                  (funcall orig-fn))))
  (popper-echo-mode +1))

;; (use-package edwina
;;   :ensure t
;;   :config
;;   (setq display-buffer-base-action '(display-buffer-below-selected))
;;   (edwina-setup-dwm-keys)
;;   (edwina-mode 1))

;; (defun mu4e--main-action-str (str &optional func-or-shortcut))
;; (defun evil-collection-mu4e-update-main-view@override())
;; (advice-add 'evil-collection-mu4e-update-main-view :override #'evil-collection-mu4e-update-main-view@override)
(use-package mu4e
  ;; :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)
  (setq message-send-mail-function 'smtpmail-send-it)
  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail")

  (setq mu4e-contexts
        (list
         ;; Work account
         (make-mu4e-context
          :name "Personal"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "dongyuanda@gmail.com")
                  (user-full-name    . "Yuanda Dong")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/gmail/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/gmail/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/gmail/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/gmail/[Gmail]/Bin")))))

  (setq mu4e-maildir-shortcuts
      '(("/gmail/Inbox"             . ?i)
        ("/gmail/[Gmail]/Sent Mail" . ?s)
        ("/gmail/[Gmail]/Bin"     . ?t)
        ("/gmail/[Gmail]/Drafts"    . ?d)
        ("/gmail/[Gmail]/All Mail"  . ?a)))
  ;; (mu4e t)

  :custom
  ;; (mu4e-display-update-status-in-modeline t)
  (mu4e-hide-index-messages t)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-format-flowed t)
  (mu4e-compose-signature (concat
  "Yuanda\n"
  "🐸")
)
  ;; (mu4e-compose-context-policy )
)

;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
;; (defun jcs-view-in-eww (msg)
;;   (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
;; (add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html"))
  ;; (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(use-package perspective
  ;; :bind
  ;; ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))
(define-key evil-normal-state-map (kbd "SPC P") 'perspective-map)

(use-package tramp
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:aws_ubuntu:")
                     "direct-async-process" t))
  ;; Tips to speed up connections
  (setq tramp-verbose 0)
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil))

(provide 'init-manager)
;;; init-manager.el ends here
