;; Fundamental settings

;; ------------------------------ Setting up ------------------------------

;; don't save custom variables
(setq custom-file (make-temp-file ""))

(setq make-backup-files nil        ; don't create backup~ files
      auto-save-default nil        ; don't create #autosave# files
      inhibit-startup-message t    ; don't show welcome screen
      ring-bell-function 'ignore   ; don't make beep sounds
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

(progn ;; Hide some builtin UI
    (menu-bar-mode -1)
    ;; GUI
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (blink-cursor-mode -1)
    ;; TUI (?)
    (setq visible-cursor nil))

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

;; ------------------------------ Boostrapping ------------------------------

;; (progn ;; `package.el' + `use-package'
;;     (setq package-user-dir (concat user-emacs-directory "elpa")
;;           package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
;;                              ("melpa" . "https://melpa.org/packages/")
;;                              ("cselpa" . "https://elpa.thecybershadow.net/packages/")))
;;     (setq use-package-always-ensure t)
;;
;;     ;; initialize packages (only once)
;;     (unless (bound-and-true-p package--initialized)
;;         (package-initialize))
;;
;;     ;; install `use-package' if needed
;;     (unless (package-installed-p 'use-package)
;;         (package-refresh-contents)
;;         (package-install 'use-package))
;;     (require 'use-package))

(progn ;; `straight.el'
    ;; TODO: add `straight` and `eva`
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
        (unless (file-exists-p bootstrap-file)
            (with-current-buffer
                    (url-retrieve-synchronously
                     "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                     'silent 'inhibit-cookies)
                (goto-char (point-max))
                (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage)))

(progn ;; `straight.el' + `use-package'
    (straight-use-package 'use-package)
    (setq straight-use-package-by-default t))

(progn ;; `use-package'
    (setq use-package-expand-minimally t
          use-package-compute-statistics t
          use-package-enable-imenu-support t)
    (require 'bind-key)
    (use-package diminish :defer t))

;; Update packages automatically (every 7 days)
(use-package auto-package-update
    :init
    (setq auto-package-update-last-update-day-filename
          (concat user-emacs-directory "tmp/.last-package-update-day"))
    :config
    (setq auto-package-update-delete-old-versions t
          auto-package-update-interval 7)
    (auto-package-update-maybe))

;; `quelpa` boostrapping: https://github.com/quelpa/quelpa
;; TODO: Prefer `straight`
(unless (package-installed-p 'quelpa)
    (with-temp-buffer
        (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
        (eval-buffer)
        (quelpa-self-upgrade)))

;; `:quelpa` keyword on `use-package`
(progn (quelpa
        '(quelpa-use-package
          :fetcher git
          :url "https://github.com/quelpa/quelpa-use-package.git"))
       (require 'quelpa-use-package))


;; ------------------------------ View settings ------------------------------

(defun toy/load-theme-packages ()
    (interactive)

    ;; color scheme
    (use-package doom-themes
        :config
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t
              doom-themes-enable-italic t)
        ;; flush mode-line on errors
        ;; (doom-themes-visual-bell-config)

        (load-theme 'doom-opera t)

        ;; FIXME: Loading the `local` package here
        (require 'smyx-theme)
        (load-theme 'smyx t))

    ;; (use-package darkokai-theme
    ;;     :config (load-theme 'darkokai t))

    ;; modeline
    (use-package doom-modeline
        ;; https://seagle0128.github.io/doom-modeline/
        :config
        ;; icons, please
        (setq doom-modeline-icon (display-graphic-p)
              doom-modeline-major-mode-icon (display-graphic-p))
        (setq doom-modeline-height 18           ;; can't be smaller than this..
              doom-modeline-buffer-encoding nil ;; 'cause it's always UTF-8
              doom-modeline-minor-modes nil     ;; if `t`, should `:diminish` minor modes
              doom-modeline-buffer-file-name-style 'truncate-upto-project)
        (doom-modeline-mode))

    (use-package wc-mode :defer t)

    ;; Show git status on line numbers
    (use-package git-gutter
        :if (not (display-graphic-p))
        :config (global-git-gutter-mode)
        :custom
        (git-gutter:modified-sign "~")
        (git-gutter:added-sign    "+")
        (git-gutter:deleted-sign  "-")
        :custom-face
        (git-gutter:modified ((t (:background "#c0b18b" :foreground "#2f2f2f"))))
        (git-gutter:added    ((t (:background "#84edb9" :foreground "#2f2f2f"))))
        (git-gutter:deleted  ((t (:background "#d75f5f" :foreground "#2f2f2f")))))

    (use-package git-gutter-fringe
        :if (display-graphic-p)
        :config (global-git-gutter-mode)
        ;; TODO: mention what `fringe` is

        :custom
        (git-gutter-fr:modified-sign "~")
        (git-gutter-fr:added-sign    "+")
        (git-gutter-fr:deleted-sign  "-")

        :custom-face
        (git-gutter-fr:modified ((t (:background "#c0b18b" :foreground "#2f2f2f"))))
        (git-gutter-fr:added    ((t (:background "#84edb9" :foreground "#2f2f2f"))))
        (git-gutter-fr:deleted  ((t (:background "#d75f5f" :foreground "#2f2f2f")))))

    (use-package rainbow-delimiters
        :config
        (define-globalized-minor-mode toy/global-rainbow-delimiters-mode rainbow-delimiters-mode
            (lambda () (rainbow-delimiters-mode 1)))
        (toy/global-rainbow-delimiters-mode 1)
        )

    ;; show color codes like this: #c0b18b
    (use-package rainbow-mode
        :config
        (define-globalized-minor-mode toy/global-rainbow-mode rainbow-mode
            (lambda () (rainbow-mode 1)))
        (toy/global-rainbow-mode 1)
        )

    (use-package hl-todo ;; highlight TODO, FIXME, etc.
        ;; https://github.com/tarsius/hl-todo
        :config
        (setq hl-todo-highlight-punctuation ":"
              hl-todo-keyword-faces
              `(("TODO"       warning bold)
                ("FIXME"      error bold)
                ("HACK"       font-lock-constant-face bold)
                ("REVIEW"     font-lock-keyword-face bold)
                ("NOTE"       success bold)
                ("WIP"        font-lock-keyword-face bold)
                ("REMARK"     success bold)
                ("DEPRECATED" font-lock-doc-face bold)))
        (global-hl-todo-mode 1))
    )

;; ------------------------------ Evil ------------------------------

(use-package evil
    :init
    ;; do not bind `C-z` to `evil-emacs-state`
    (setq evil-toggle-key "")
    ;; [evil-collection] do not load `evil-keybindings.el`
    (setq evil-want-keybinding nil)

    ;; NOTE: In terminal, if we bind `C-i`, it also binds `<TAB>`.
    ;; If you prefer to bind `TAB` to other functionality, do:
    ;; (setq evil-want-C-i-jump (display-graphic-p))
    ;; But basically prefer GUI for `org-mode` etc.

    ;; configure
    (setq evil-want-C-u-delete t     ; use C-u for deleting in insert mode
          evil-want-C-u-scroll t     ; use C-u for scrolling in normal mode
          evil-want-Y-yank-to-eol t  ; map `Y` to `y$`
          evil-move-cursor-back t    ; move cursor back when exiting insert mode (like Vim)
          evil-search-module 'evil-search  ; `evil-search` seem to highlight better
          )
    :config
    (evil-mode 1))

(use-package evil-collection
    :after evil
    :config
    ;; I do not want to use `evil-collection` in e.g. `neotree` because
    ;; `evil-define-key` don't make sense (without making a higher precedence keymap)
    ;; (evil-collection-init)
    ;; So let's enable `evil-collection` mappings by package

    (with-eval-after-load 'dired (evil-collection-dired-setup))
    (with-eval-after-load 'company (evil-collection-company-setup))
    (with-eval-after-load 'eww (evil-collection-eww-setup))
    (with-eval-after-load 'elfeed (evil-collection-elfeed-setup))
    (with-eval-after-load 'markdown-mode (evil-collection-markdown-mode-setup))

    ;; TODO: needed?
    ;; (with-eval-after-load 'consult (evil-collection-consult-setup))
    ;; (with-eval-after-load 'embark (evil-collection-embark-setup))

    (with-eval-after-load 'magit
        (evil-define-key 'normal magit-mode-map
            ;; scolling
            "zz" #'evil-scroll-line-to-center
            "z-" #'evil-scroll-line-to-bottom
            (kbd "z RET") #'evil-scroll-line-to-top
            ;; visit thing at point in other window
            (kbd "SPC RET") #'magit-diff-visit-worktree-file-other-window)

        ;; scroll to the hunk after `gj` or `gk`
        (advice-add 'magit-section-forward :after
                    (lambda (&rest x) (evil-scroll-line-to-top (line-number-at-pos))))
        (advice-add 'magit-section-backward :after
                    (lambda (&rest x) (evil-scroll-line-to-top (line-number-at-pos))))
        (advice-add 'magit-section-forward-sibling :after
                    (lambda (&rest x) (evil-scroll-line-to-top (line-number-at-pos))))
        (advice-add 'magit-section-backward-sibling :after
                    (lambda (&rest x) (evil-scroll-line-to-top (line-number-at-pos))))

        ;; [magit] add fold mappings (`z1`, `z2`, .., `za`, ..)
        (setq evil-collection-magit-use-z-for-folds t)

        ;; NOTE: magit changes `g` to `gr` (`magit-refresh`)

        (evil-collection-magit-setup))
    )

(progn ;; Interactive commands
    ;; use `:ed` to open `init.el`
    (evil-ex-define-cmd "ed" (lambda () (interactive) (evil-edit (concat user-emacs-directory "init.el"))))
    ;; use `:s` to edit `init.el`
    (evil-ex-define-cmd "s" (lambda () (interactive) (load-file (concat user-emacs-directory "init.el"))))
    ;; close buffer (without closing window). alternative: `C-x k RET`
    (evil-ex-define-cmd "Bd" #'kill-this-buffer)
    (evil-ex-define-cmd "BD" #'kill-this-buffer)
    ;; horizontal split
    (evil-ex-define-cmd "hs" #'evil-window-split))

;; Enable redo with evil
(use-package undo-tree
    :init
    (evil-set-undo-system 'undo-tree)
    (global-undo-tree-mode))

(use-package evil-surround
    :config (global-evil-surround-mode))

(use-package expand-region
    :config
    (evil-define-key 'visual 'global
        "v" #'er/expand-region
        "V" #'er/contract-region))

;; ;; TODO:
;; (use-package evil-textobj-parameter
;;     ;; https://github.com/bevyengine/bevy/tree/main/crates
;;     :straight (el-patch :type git :host github :repo "Cj-bc/evil-textobj-parameter")
;;     )

;; ------------------------------ Help ------------------------------

(use-package which-key
    :init
    ;; a) show hints immediately
    (setq which-key-idle-delay 0.01
          which-key-idle-secondary-delay 0.01)

    ;; b) always press `C-h` to trigger which-key
    ;; (setq which-key-show-early-on-C-h t
    ;;       which-key-idle-delay 10000
    ;;       which-key-idle-secondary-delay 0.05)

    :config
    ;; (which-key-setup-minibuffer)
    (define-key help-map (kbd "M") 'which-key-show-major-mode)
    (which-key-mode))

;; Improved version of *Help*
(use-package helpful
    :bind
    ([remap describe-command] . helpful-command)
    ([remap describe-key] . helpful-key)
    :init
    ;; press `q` or `<escape>` to quit (kill) the buffer
    (evil-define-key 'normal helpful-mode-map "q" #'kill-this-buffer)
    ;; press `K` to see the help!
    (evil-define-key 'normal 'global "K" #'helpful-at-point)
    )

;; ------------------------------ GUI/Terminal ------------------------------

;; [GUI] Icons
(use-package all-the-icons :if (display-graphic-p))

;; [GUI] Font
(when (display-graphic-p)
    (set-face-attribute 'default nil :family "Menlo" :height 120)
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      (font-spec :family "Hiragino Kaku Gothic ProN"))
    (add-to-list 'face-font-rescale-alist
                 '(".*Hiragino Kaku Gothic ProN.*" . 1.2)))

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

;; [Terminal] Add command to copy to clipboard
(use-package clipetty
    :after evil
    :config
    ;; copy to system clipboard with `:copy`
    ;; TODO: support clipboard register
    (evil-ex-define-cmd "copy" #'clipetty-kill-ring-save))

;; ------------------------------ Dashboard ------------------------------

(use-package dashboard
    :config
    (setq dashboard-items '(
                            (projects . 30)
                            (recents  . 5)
                            (bookmarks . 5)
                            (agenda . 5)
                            (registers . 5)
                            ))
    (setq dashboard-set-heading-icons (display-graphic-p)
          dashboard-set-file-icons (display-graphic-p))

    (dashboard-setup-startup-hook))

;; use `dashborad` in `emacs -client`
(let (d (get-buffer dashboard-buffer-name))
    (when d
        (setq initial-buffer-choice d))
    )

;; ------------------------------ ELisp ------------------------------

(progn ;; ELisp
    (setq-default lisp-body-indent 4    ; I need this
                  indent-tabs-mode nil  ;
                  tab-width 4           ; display tab with a width of 4
                  )

    ;; enable folding (`zr` to open all, `zm` to fold all, `za` to toggle)
    (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

    (use-package aggressive-indent
        :hook ((emacs-lisp-mode scheme-mode) . aggressive-indent-mode)))

