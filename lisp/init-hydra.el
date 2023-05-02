;;; init-hydra.el -- User init file for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package hydra
  :defer t)

(defhydra hydra-zoom ()
    "
^Zoom^                 ^Other
^^^^^^^--------------------------
[_j_/_k_] zoom in/out  [_q_] quit
[_0_]^^   reset zoom
"
  ("j" text-scale-increase)
  ("k" text-scale-decrease)
  ("0" text-scale-adjust)
  ("q" nil :exit t))



(defhydra writeroom-buffer-width ()
    "
^Width^        ^Other
^^^^^^^^-----------------------
[_j_] enlarge  [_r_/_0_] adjust
[_k_] shrink   [_q_]^^   quit
"
  ("q" nil :exit t)
  ("j" writeroom-increase-width)
  ("k" writeroom-decrease-width)
  ("r" writeroom-adjust-width)
  ("0" writeroom-adjust-width))


(defhydra mu4e-headers-split-adjust-width ()
  ("q" nil :exit t)
  ("j" mu4e-headers-split-view-shrink "shrink")
  ("k" mu4e-headers-split-view-grow "enlarge"))



(defhydra windows-adjust-size ()
    "
^Zoom^                                ^Other
^^^^^^^-----------------------------------------
[_j_/_k_] shrink/enlarge vertically   [_q_] quit
[_h_/_l_] shrink/enlarge horizontally
"
  ("q" nil :exit t)
  ("h" shrink-window-horizontally)
  ("j" enlarge-window)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally))


(set-frame-parameter nil 'alpha-background 100) ; For current frame
(defun my/increase-frame-alpha-background ()
  "Increase current frame’s alpha background."
  (interactive)
  (set-frame-parameter nil
                       'alpha-background
                        (min 100
                             (+ (frame-parameter nil 'alpha-background) 5)))
  (message "%s" (frame-parameter nil 'alpha-background)))

(defun my/decrease-frame-alpha-background ()
  "Decrease current frame’s alpha background."
  (interactive)
  (set-frame-parameter nil
                       'alpha-background
                        (max 0
                             (- (frame-parameter nil 'alpha-background) 5)))
  (message "%s" (frame-parameter nil 'alpha-background)))

(defhydra my/modify-frame-alpha-background ()

  "
^Transparency^              ^Other^
^^^^^^^^^^^^^^------------------------
[_j_] decrease alpha [_q_] quit
[_k_] increase alpha
"
  ("q" nil :exit t)
  ("j" my/decrease-frame-alpha-background)
  ("k" my/increase-frame-alpha-background))

(provide 'init-hydra)

;;; init-hydra.el ends here
