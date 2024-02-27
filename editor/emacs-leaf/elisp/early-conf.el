;; -*- lexical-binding: t -*-
;;
;; Emacs configuration with no dependency

;; don't save custom variables
(setq custom-file (make-temp-file ""))

(setq make-backup-files nil        ; don't create backup~ files
      auto-save-default nil        ; don't create #autosave# files
      )

(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")

(progn ;; UTF-8
    (set-charset-priority 'unicode)
    (prefer-coding-system 'utf-8)
    (setq locale-coding-system 'utf-8)
    (set-language-environment 'utf-8)
    (set-default-coding-systems 'utf-8)
    ;; it modifies the buffer
    ;; (set-buffer-file-coding-system 'utf-8)
    ;; it requires flusing
    ;; (set-terminal-coding-system 'utf-8)
    (set-clipboard-coding-system 'utf-8)
    (set-file-name-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (modify-coding-system-alist 'process "*" 'utf-8))

(progn ;; Show more
    ;; show line numbers
    (global-display-line-numbers-mode)

    ;; highlight current line
    ;; (global-hl-line-mode t)

    ;; show trailing whitespaces
    (setq-default show-trailing-whitespace t)

    ;; show tabs
    (require 'whitespace)
    (whitespace-mode 1)
    (setq whitespace-style '(tabs  tab-mark))

    (progn ;; show matching parentheses
        (setq-default show-paren-delay 0)
        (show-paren-mode 1))

    ;; show `line:column` in the modeline
    (column-number-mode))

;; [GUI]
(set-cursor-color "#8fee96")
(set-fringe-mode 10)

;; Scroll like Vim
(setq scroll-preserve-screen-position t  ; keep the cursor position when scrolling
      scroll-conservatively 100          ; scroll by lines, not by a half page
      scroll-margin 3                    ; scroll keeping the margins
      )

;; ------------------------------ Builtin packages ------------------------------

;; TODO: put them in leaf

;; put auto-generated files in `tmp` directory (builtin packages)
(setq recentf-save-file (concat user-emacs-directory "tmp/recentf")
      save-place-file (concat user-emacs-directory "tmp/places")
      savehist-file (concat user-emacs-directory "tmp/history")
      auto-save-list-file-prefix (concat user-emacs-directory "tmp/auto-save-list"))

(progn ;; save command history
    (setq history-length 1000
          history-delete-duplicates t)
    (savehist-mode))

(progn ;; sync buffers to storage per second
    (setq auto-revert-interval 1)
    (global-auto-revert-mode))

;; save cursor positions per file
(save-place-mode 1)

(progn ;; HACK: re-center curspr position with `save-place-mode`:
    ;; https://www.reddit.com/r/emacs/comments/b2lokk/recenter_saved_place/
    (defun toy/fix-save-place ()
        "Force windows to recenter current line (with saved position)."
        (run-with-timer 0 nil
                        (lambda (buf)
                            (when (buffer-live-p buf)
                                (dolist (win (get-buffer-window-list buf nil t))
                                    (with-selected-window win (recenter)))))
                        (current-buffer)))
    (add-hook 'find-file-hook #'toy/fix-save-place))

(progn ;; keep a list of recently opened files
    (setq recentf-max-saved-items 1000)
    (recentf-mode 1))

(progn ;; show duplicate file names as `file<parent-directory>`
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
    (require 'uniquify))

;; ------------------------------ GUI/Terminal ------------------------------

;; [GUI] Font
(when (display-graphic-p)
    ;; (set-face-attribute 'default nil :family "roboto-mono" :height 110)
    ;; (set-face-attribute 'default nil :family "roboto-mono")
    ;; (set-face-attribute 'default nil :family "Noto Sans Mono")

    ;; TODO: setup monospaced font
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      ;; TODO: fallback
                      ;; (font-spec :family "Hiragino Kaku Gothic ProN")
                      (font-spec :family "Noto Sans Mono CJK JP"))

    ;; FIXME: proper way to align org tables?
    (setq face-font-rescale-alist
          '(("Noto Sans Mono CJK JP" . 1.25)))

    ;; (setq face-font-rescale-alist
    ;;       '(("roboto.*" . 1.0)))
    )

;; If on terminal
(when (not (display-graphic-p))
    ;; Two exclusive options:
    ;; 1. use left click to move cursor:
    (xterm-mouse-mode 1)
    ;; 2. use left click to select (and copy):
    ;; (xterm-mouse-mode -1)

    ;; use mouse wheel for scrolling
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; ------------------------------ ELisp ------------------------------

(setq-default lisp-body-indent 4    ; I need this
              indent-tabs-mode nil  ;
              tab-width 4           ; display tab with a width of 4
              )

