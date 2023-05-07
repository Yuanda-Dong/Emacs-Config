;;; init-keybinds.el -- User init file for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; (use-package general
;;   :demand t
;;   :init
;;   (general-auto-unbind-keys)
;;   :config

(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(general-create-definer me/undefine
  :keymaps 'override
  :states '(normal emacs))
(general-create-definer me/evil-normal
  :states '(normal))
(general-create-definer me/evil-motion
  :states '(motion))

(general-create-definer me/leader-key
  :states '(motion normal insert visual emacs)
  :keymaps 'override

  :prefix "SPC"
  :global-prefix "C-SPC")

(general-create-definer me/mode-leader-key
  :states '(motion normal insert visual emacs)
  :keymaps 'override
  :prefix ","
  :global-prefix "M-m")

(general-create-definer me/ctrl-c-keys
  :prefix "C-c")
;; (me/undefine
;;   "C-j" nil)
;; (global-unset-key (kbd "C-j"))
;; (global-unset-key (kbd "C-k"))
(general-def 'override
  "C-M-u" 'universal-argument
  "M-s" 'string-inflection-all-cycle
  "C-<up>" 'transpose-line-up
  "C-<down>" 'transpose-line-down
  "C-x a a" 'align-regexp
  "M-t" 'transpose-words
  "<escape>" 'keyboard-escape-quit
  ;; "<f2>" 'vterm-toggle
  ;; "C-<f2>" 'vterm-toggle-cd
  "C-x b" 'switch-to-buffer
  "C-x C-b" 'switch-to-buffer
  "C-x f" 'find-file
  "<Tools>" 'visual-fill-column-mode
  "C--" 'text-scale-decrease
  "C-=" 'text-scale-increase
  "C-s" 'consult-line
  )

(general-def insert corfu-map
  "C-j"  'corfu-next
  "C-k"  'corfu-previous
  ;; "<escape>" 'corfu-quit
  "s-SPC" 'corfu-insert-separator
  ;; "m-SPC" nil
)

(general-def normal prog-mode-map
  "K" 'lsp-describe-thing-at-point
  "gr" 'lsp-ui-peek-find-references
  )

(general-def visual evil-mc-key-map
  "A" 'evil-mc-make-cursor-in-visual-selection-end
  "I" 'evil-mc-make-cursor-in-visual-selection-beg
)

(me/leader-key
  "j"   '(:ignore t :which-key "jump")
  "jd" 'dirvish
  "jD" 'dired-jump-other-window
  "jt" 'vterm
  "jm" 'mu4e
  "je" 'mu4e
  "jc" 'gptel
  "jl" 'my/leetcode-view-problem

  ";" '(dashboard-open :which-key "dashboard")
  "b"   '(:ignore t :which-key "buffer")
  "bb" 'switch-to-previous-buffer
  "bl" 'bufler
  "bo"  '(consult-buffer :which-key "consult-buffer")
  "bp"  '(consult-project-buffer :which-key "consult-project-buffer")
  "bs"  'scratch-buffer
  "bm" 'switch-to-messages-buffer
  "bk" 'kill-buffer
  "e"  '(dirvish-side :which-key "dirvish side")

  "f"   '(:ignore t :which-key "files")
  "ff" 'find-file
  "fF"  'consult-find
  "fr"  'consult-recent-file
  "fR"  'revert-buffer
  "rg" 'consult-ripgrep
  "/" '(evilnc-comment-or-uncomment-lines :which-key "comment")
  ;; "p" '(:keymap projectile-command-map :which-key "projectile" :package projectile)
  "p" '(:keymap project-prefix-map :which-key "project")

  "n" '(:ignore t :which-key "org-roam")
  "n l" 'org-roam-buffer-toggle
  "n f" 'org-roam-node-find
  "n i" 'org-roam-node-insert
  "n I" 'org-roam-node-insert-immediate

  "n d" '(:keymap org-roam-dailies-map  :which-key "dailies" :package org-roam)
  ;; "n d T" 'org-roam-dailies-capture-tomorrow
  ;; "n d Y" 'org-roam-dailies-capture-yesterday
  "t" '(:ignore t :which-key "toggle")
  "tT" 'consult-theme
  "td" '(:ignore t :which-key "debug")
  "tde" 'toggle-debug-on-error
  "tdq" 'toggle-debug-on-quit
  "tk" '+toggle-keycast
  "tt" 'my/modify-frame-alpha-background/body

  "T" '(:ignore t :which-key "text")
  "Td" 'downcase-region
  "Tu" 'upcase-region
  "Tz" 'hydra-zoom/body
  "Ts" 'string-inflection-all-cycle

  "w" '(:ignore t :which-key "window")
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wv" 'evil-window-vsplit
  "ws" 'evil-window-split
  "wc" 'evil-window-delete
  "wH" 'evil-window-move-far-left
  "wL" 'evil-window-move-far-right
  "wJ" 'evil-window-move-very-bottom
  "wK" 'evil-window-move-very-top
  "w." 'windows-adjust-size/body
  "wC" 'kill-buffer-and-window
  "wo" 'other-window
  "wD" 'delete-other-windows
  "w<" 'winner-undo
  "w>" 'winner-redo
  "ww" '(:ignore t :which-key "writeroom")
  "ww." 'writeroom-buffer-width/body
  "www" 'writeroom-mode

  "h" '(:ignore t :which-key "help")
  "hf" 'helpful-callable
  "hF" 'helpful-function
  "hv" 'helpful-variable
  "hk" 'helpful-key
  "hc" 'helpful-command
  "hd" 'helpful-at-point
  "hp" 'describe-package
  "hs" 'helpful-symbol
  "hm" 'helpful-macro
  "hM" 'helpful-mode
  "hi" 'info
  "hI" 'info-display-manual
  "TAB" 'hs-toggle-hiding
  ;; "hk" 'which-key-show-top-level
  ;; "hf" 'helpful--tree-any-p
  "ie" 'emoji-insert
  )

(me/leader-key
 :infix   "g"
 :packages 'magit
 ""   '(:ignore t :wk "git")
 "b"  #'magit-blame
 "c"  #'magit-clone
 "d"  #'magit-dispatch
 "i"  #'magit-init
 "s"  #'magit-status
 "l"  #'magit-log
 "y"  #'my/yadm
 "S"  #'magit-stage-file
 "U"  #'magit-unstage-file
 "f"  '(:ignore t :wk "file")
 "fd" #'magit-diff
 "fc" #'magit-file-checkout
 "fl" #'magit-file-dispatch
 "fF" #'magit-find-file)


(general-def normal dirvish-mode-map
  "h"  'dired-up-directory
  "<left>"  'dired-up-directory
  "l"  'dired-find-file
  "<right>"  'dired-find-file
  "s"  'dirvish-quicksort
  "a"  'dirvish-quick-access
  "g"  'dirvish-quick-access
  "q"  'dirvish-quit
  "f"  'dirvish-file-info-menu
  "p"  'dirvish-yank-menu
  "C-c f" 'dirvish-fd
  "TAB" 'dirvish-subtree-toggle
  "N"   'dirvish-narrow
  "v"    'dirvish-vc-menu
  )

;; (setq mouse-1-click-follows-link nil)
(define-key dirvish-mode-map (kbd "<mouse-1>") nil)
(define-key dirvish-mode-map [double-mouse-1] 'dired-find-file)
(define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)
(define-key dirvish-mode-map (kbd "<mouse-8>") 'dired-up-directory)
(define-key dirvish-mode-map (kbd "<mouse-9>") 'dirvish-history-go-forward)
(define-key dirvish-mode-map (kbd "<mouse-10>") 'dirvish-quick-access)
;; (define-key dirvish-mode-map (kbd "<mouse-3>") 'dired-mouse-find-file)


(me/mode-leader-key dirvish-mode-map
  "e"   'dirvish-emerge-menu
  "f"   'dirvish-fd
  "j"   'dirvish-fd-jump
  "l"   '(:ignore t :which-key "layout")
  "ls"  'dirvish-layout-switch
  "lt"  'dirvish-layout-toggle
  "m"   'dirvish-mark-menu
  "s"   'dirvish-ls-switches-menu
  "S"   'dirvish-setup-menu
  "h"   '(:ignore t :which-key "history")
  "hp"  'dirvish-history-go-backward
  "hn"  'dirvish-history-go-forward
  "hj"  'dirvish-history-jump
  "hl"  'dirvish-history-last
  "v"   'dirvish-vc-menu
  )

(me/evil-motion
  :keymaps  'bufler-list-mode-map
  :packages 'bufler
  "?"   'hydra:bufler/body
  "<escape>" 'keyboard-quit
  "RET" 'bufler-list-buffer-switch
  )

(me/evil-normal
  "m" '(:keymap evil-mc-cursors-map :package evil-mc)
  )



;;; VTERM
(me/mode-leader-key vterm-mode-map
  "c" #'multi-vterm
  "C" #'multi-vterm-project
  "d" #'multi-vterm-dedicated-toggle
  "n" #'multi-vterm-next
  "p" #'multi-vterm-prev)

(general-def vterm-mode-map
  "M-`" 'popper-cycle
  )



(provide 'init-keybinds)

;;; init-keybinds.el ends here
