;;; init-ui.el -- User init file for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar me/default-font-size 150)
(defvar me/default-variable-font-size 180)
;; (defvar me/fix-font-name "Je")
(defvar me/default-font-name "JetBrainsMono Nerd Font")
(defvar me/variable-font-name "Vollkorn")

(defun me/set-font-faces ()
  (set-face-attribute 'default nil :font me/default-font-name :height me/default-font-size)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font me/default-font-name :height 1.0)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font me/variable-font-name :height me/default-variable-font-size)
)

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (setq doom-modeline-icon t)
;;                 ;; (setq dashboard-display-icons-p t)
;;                 (with-selected-frame frame
;;                   (me/set-font-faces))))
;;     (me/set-font-faces))
(me/set-font-faces)



(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))

;; Fringe
;; (set-fringe-style 30)
;; (set-fringe-mode 15)


;; Use icons
(use-package all-the-icons)
  ;; :if (display-graphic-p))


(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Global settings (defaults)
  (load-theme 'doom-ayu-mirage t)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Modeline
(setq display-time-format "%l:%M %p %b %d"
      display-time-default-load-average nil)
(display-time-mode 1)
(column-number-mode)
(defun modeline-contitional-buffer-encoding ()
  "Hide \"LF UTF-8\" in modeline.

        It is expected of files to be encoded with LF UTF-8, so only show
        the encoding in the modeline if the encoding is worth notifying
        the user."
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))
(add-hook 'after-change-major-mode-hook #'modeline-contitional-buffer-encoding)

;; hide pesky minor modes
(use-package minions
 :hook (doom-modeline-mode . minions-mode))


(use-package doom-modeline
  :init (doom-modeline-mode 1)
    :custom
    (doom-modeline-enable-word-count nil)
    (doom-modeline-modal nil)
    (doom-modeline-height 15)
    (doom-modeline-bar-width 4)
    (doom-modeline-hud nil)
    (doom-modeline-lsp t)
    (doom-modeline-time-icon nil)
    ;; (doom-modeline-github nil)
    (doom-modeline-mu4e nil)
    (doom-modeline-irc nil)
    (doom-modeline-minor-modes t)
    ;; (doom-modeline-persp-name nil)
    (doom-modeline-buffer-file-name-style 'truncate-except-project)
    (doom-modeline-major-mode-icon nil)
    (doom-modeline-highlight-modified-buffer-name t)
    (doom-modeline-buffer-state-icon nil))


;; Dashboard
(use-package dashboard
  :init   (setq dashboard-navigator-buttons `(((,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "mark-github"      :height 1.0 :v-adjust  0.0) "★")
                                                "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                               (,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "heart"            :height 1.1 :v-adjust  0.0) "♥")
                                                "Stars" "Show stars" (lambda (&rest _) (browse-url stars-url)))
                                               (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "report_problem" :height 1.1 :v-adjust -0.2) "⚑")
                                                "Issue" "Report issue" (lambda (&rest _) (browse-url issue-url)) warning)
                                               (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "update"         :height 1.1 :v-adjust -0.2) "♺")
                                                "Update" "Update packages synchronously" (lambda (&rest _) (package-update-all nil)) success))))
  :config
  (setq dashboard-banner-logo-title "My Emacs"
        dashboard-startup-banner    "/home/me/Pictures/swappy-20230225-063700.png"
        dashboard-show-shortcuts    t
        dashboard-set-navigator     t
        dashboard-set-heading-icons t
        dashboard-set-file-icons    t
        dashboard-set-init-info nil
        dashboard-set-footer nil
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*")))
  ;;  dashboard-projects-switch-function 'projectile-switch-project-by-name)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (agenda   . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook)
  :custom
  dashboard-projects-backend 'project-el
  )

(use-package keycast
  ;; uncomment this if you're using straight.el
  ;:straight t
  :config
  (defun +toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast OFF"))
      (add-to-list 'global-mode-string '("" keycast-mode-line " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast ON"))))
;; :config (keycast-mode-line-mode 1))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-sort-uppercase-first nil)
  (which-key-idle-delay 0.5))

;;** =hl-todo= - Highlight TODOs in comments
(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (text-mode . enable-hl-todo-unless-org-buffer))
  :preface
  (defun enable-hl-todo-unless-org-buffer ()
    (unless (derived-mode-p 'org-mode)
      (hl-todo-mode)))
  :custom
  (hl-todo-keyword-faces
   (seq-map (lambda (it) (cons it 'hl-todo))
            '("TODO"
              "NEXT"
              "HACK"
              "FIXME"
              "KLUDGE"
              "PATCH"
              "NOTE"))))
(use-package mixed-pitch
  :after org
  :hook
  (org-mode           . mixed-pitch-mode)
  (markdown-mode . mixed-pitch-mode)
  ;; (emms-browser-mode  . mixed-pitch-mode)
  ;; (emms-playlist-mode . mixed-pitch-mode)
  :config
  (add-hook 'org-agenda-mode-hook (lambda () (mixed-pitch-mode -1)))
  :custom
  (mixed-pitch-set-height t)
  )

(use-package info-colors
  :commands info-colors-fontify-node
  :hook (Info-selection . info-colors-fontify-node)
  :hook (Info-mode      . mixed-pitch-mode))


(provide 'init-ui)

;;; init-ui.el ends here
