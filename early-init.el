;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Put the ELN cache directory in an XDG-conformant place
(when (boundp 'native-comp-eln-load-path)
  (let ((cache-dir "~/.cache/emacs/eln-cache/"))
    (mkdir cache-dir t) (add-to-list 'native-comp-eln-load-path cache-dir)))

;; Faster to disable these here (before they've been initialized)
(custom-set-variables '(menu-bar-mode nil)
                      '(tool-bar-mode . nil) '(scroll-bar-mode nil))
(modify-all-frames-parameters '((vertical-scroll-bars)))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))

(provide 'early-init)
;;; early-init.el ends here
