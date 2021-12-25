;; ------------------------------ Hydra ------------------------------

;; builtin!
(require 'winner)
(winner-mode 1)

(leaf windswap
      ;; https://github.com/amnn/windswap
      ;; windswap-left|right|up|down
      :commands (windswap-up windswap-down windswap-left windswap-right))

(defun toy/tab-move-right ()
    (interactive)
    (let* ((ix (tab-bar--current-tab-index))
           (n-tabs (length (funcall tab-bar-tabs-function)))
           (next-ix (mod (+ ix 1) n-tabs)))
        ;; use 1-based index
        (tab-bar-move-tab-to (+ 1 next-ix))))

(defun toy/tab-move-left ()
    (interactive)
    (let* ((ix (tab-bar--current-tab-index))
           (n-tabs (length (funcall tab-bar-tabs-function)))
           (next-ix (mod (+ ix n-tabs -1) n-tabs)))
        ;; use 1-based index
        (tab-bar-move-tab-to (+ 1 next-ix))))

(defvar toy/expand-unit 5)

(defhydra toy/hydra-window (:color red :hint nil)
    "
hjkl: focus     rR: rotate          t: tab (prefix)
HJKL: resize    =: equlize          C-h, C-l: tab focus
wasd: split     c/q: close          1-9: move to tab
WASD: swap      x: kill, X: both    u: undo window change
"

    ("u" winner-undo)
    ;; doesn't work
    ;; ("C-r" winner-redo)

    ;; switch to hydra for `tab-bar-mode` (Emacs 27)
    ;; ("t" (lambda () (interactive)
    ;;          (hydra--body-exit)
    ;;          (toy/hydra-tab/body)))

    ;; select centaur-tabs tabs
    ("1" (_fn (tab-bar-switch-to-tab 1)))
    ("2" (_fn (tab-bar-switch-to-tab 2)))
    ("3" (_fn (tab-bar-switch-to-tab 3)))
    ("4" (_fn (tab-bar-switch-to-tab 4)))
    ("5" (_fn (tab-bar-switch-to-tab 5)))
    ("6" (_fn (tab-bar-switch-to-tab 6)))
    ("7" (_fn (tab-bar-switch-to-tab 7)))
    ("8" (_fn (tab-bar-switch-to-tab 8)))
    ("9" (_fn (tab-bar-switch-to-tab 9)))
    ;; TODO: move tab

    ;; focus
    ("h" #'evil-window-left)
    ("j" #'evil-window-down)
    ("k" #'evil-window-up)
    ("l" #'evil-window-right)

    ("]"  #'evil-forward-section-begin)
    ("["  #'evil-backward-section-begin)

    ;; enlarge/shrink
    ("H" (_fn (enlarge-window-horizontally (- toy/expand-unit))))
    ("L" (_fn (enlarge-window-horizontally toy/expand-unit)))
    ("K" (_fn (enlarge-window (- toy/expand-unit))))
    ("J" (_fn (enlarge-window toy/expand-unit)))

    ;; split
    ("w" #'toy/sp-N)
    ("a" #'toy/sp-W)
    ("s" #'evil-window-split)
    ("d" #'evil-window-vsplit)

    ;; swap to neighbor
    ("W" (_fn (windswap-up)))
    ("A" (_fn (windswap-left)))
    ("S" (_fn (windswap-down)))
    ("D" (_fn (windswap-right)))

    ;; close
    ("c" #'toy/evil-quit) ;; NOTE: defined in `keymap.el`
    ("q" #'toy/evil-quit) ;; NOTE: defined in `keymap.el`

    ;; delete
    ("x" #'kill-this-buffer)
    ("X" #'evil-delete)

    ("C-s" #'save-buffer)

    ("r" #'evil-window-rotate-downwards)
    ("R" #'evil-window-rotate-upwards)
    ("=" #'balance-windows)

    ("z" #'toy/zen)
    ("b" 'evil-buffer-new)

    ("ESC" nil))

(defhydra toy/hydra-tab (:color red :hint nil)
    "
hl      move        r   rename
HL      swap        n   new
[1-9]   focus       x   close
m[1-9]  move
"

    ("u" winner-undo)
    ;; doesn't work
    ;; ("C-r" winner-redo)

    ;; tab-bar-mode (Emacs 27)
    ;; `awesome-tab`: https://github.com/manateelazycat/awesome-tab
    ("h"  #'tab-bar-switch-to-prev-tab)
    ("l"  #'tab-bar-switch-to-next-tab)
    ("H"  #'toy/tab-move-left)
    ("L"  #'toy/tab-move-right)
    ;; TODO: swap tab

    ;; FIXME:
    ;; ("w" #'toy/hydra-window/body)
    ("w" (_fn (hydra-disable)
              (toy/hydra-window/body)))

    ("rr" #'tab-bar-rename-tab)
    ;; rename to project name
    ("rp" #'toy/set-tab-name-default) ;; NOTE: defined in `ide.el`

    ("n" #'tab-bar-new-tab)
    ;; new tab and set name
    ("N" (_fn (tab-bar-new-tab)
              (call-interactively 'tab-bar-rename-tab)))
    ("x" #'tab-bar-close-tab)

    ;; select tab
    ("1" (_fn (tab-bar-select-tab 1)))
    ("2" (_fn (tab-bar-select-tab 2)))
    ("3" (_fn (tab-bar-select-tab 3)))
    ("4" (_fn (tab-bar-select-tab 4)))
    ("5" (_fn (tab-bar-select-tab 5)))
    ("6" (_fn (tab-bar-select-tab 6)))
    ("7" (_fn (tab-bar-select-tab 7)))
    ("8" (_fn (tab-bar-select-tab 8)))
    ("9" (lambda () (interactive) (tab-bar-select-tab 9)))

    ;; move tab
    ("m1" (_fn (tab-bar-move-tab-to 1)))
    ("m2" (_fn (tab-bar-move-tab-to 2)))
    ("m3" (_fn (tab-bar-move-tab-to 3)))
    ("m4" (_fn (tab-bar-move-tab-to 4)))
    ("m5" (_fn (tab-bar-move-tab-to 5)))
    ("m6" (_fn (tab-bar-move-tab-to 6)))
    ("m7" (_fn (tab-bar-move-tab-to 7)))
    ("m8" (_fn (tab-bar-move-tab-to 8)))
    ("m9" (_fn (tab-bar-move-tab-to 9)))
    ("ESC" nil))

;; TODO:
(defhydra toy/hydra-org (:color red :hint nil)
    "
HL: change level
"
    ;; change level
    ("H" #'org-metaleft)
    ("L" #'org-metaright)
    )

;; (org-insert-heading-respect-content &optional INVISIBLE-OK)

