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

;; ------------------------------ GUI/Terminal ------------------------------

;; [GUI] Icons

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

;; ------------------------------ ELisp ------------------------------

(progn ;; ELisp
    (setq-default lisp-body-indent 4    ; I need this
                  indent-tabs-mode nil  ;
                  tab-width 4           ; display tab with a width of 4
                  )

    ;; enable folding (`zr` to open all, `zm` to fold all, `za` to toggle)
    (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

    )

;; ------------------------------ UI ------------------------------

;; Put auto-generated files in `tmp` directory
(setq projectile-cache-file (concat user-emacs-directory "tmp/projectile.cache")
      projectile-known-projects-file (concat user-emacs-directory "tmp/projectile-bookmarks.eld")
      ;; magit
      transient-history-file (concat user-emacs-directory "tmp/transient/history.el")
      transient-values-file (concat user-emacs-directory "tmp/transient/values.el")
      transient-levels-file (concat user-emacs-directory "tmp/transient/levels.el")
      ;; lsp
      lsp-session-file (concat user-emacs-directory "tmp/.lsp-session-v")
      )

;; ------------------------------ Workspace ------------------------------

(tab-bar-mode 1)

;; use project name as default tab name
(defun toy/set-tab-name-default ()
    (let ((proj-name (projectile-project-name)))
        (unless (or (= (length proj-name) 0) (string= proj-name "-"))
            (tab-bar-rename-tab proj-name))))

(advice-add 'tab-bar-new-tab :after (lambda (&rest x) (toy/set-tab-name-default)))
(add-hook 'window-setup-hook #'toy/set-tab-name-default)

;; ------------------------------ Shell ------------------------------

;; TODO: shell
(leaf vterm
    :config
    (defun toy/turn-off-chrome ()
        (hl-line-mode -1)
        (display-line-numbers-mode -1))
    :hook (vterm-mode-hook . toy/turn-off-chrome))

;; (leaf vterm-toggle
;;                                         ;j    :custom
;;     (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
;;     (vterm-toggle-scope 'projectile)
;;     ;; :bind (("C-c t" . #'vterm-toggle)
;;     ;;        :map vterm-mode-map
;;     ;;        ("s-t" . #'vterm) ; Open up new tabs quickly
;;     ;;        )
;;     )

;; ------------------------------ Language supports ------------------------------

(defun toy/lint-ja ()
    (interactive)
    ;; TODO: run only when the command is in scope
    (flycheck-disable-checker textlint-ja)
    (flycheck-define-checker textlint-en
        "A linter for text."
        :command ("textlint-editor.bash" "textlint-ja.json" source)
        :error-patterns
        ((warning line-start (file-name) ":" line ":" column ": "
                  (id (one-or-more (not (any " "))))
                  (message (one-or-more not-newline)
                           (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                  line-end))
        :modes (text-mode markdown-mode adoc-mode gfm-mode org-mode))

    (leaf flycheck-inline
        :hook (flycheck-mode-hook . flycheck-inline-mode)))

(defun toy/lint-en ()
    (interactive)
    ;; TODO: run only when the command is in scope
    (flycheck-disable-checker textlint-en)
    (flycheck-define-checker textlint-ja
        "A linter for text."
        :command ("textlint-editor.bash" "textlint-en.json" source)
        :error-patterns
        ((warning line-start (file-name) ":" line ":" column ": "
                  (id (one-or-more (not (any " "))))
                  (message (one-or-more not-newline)
                           (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                  line-end))
        :modes (text-mode markdown-mode adoc-mode gfm-mode org-mode))

    (leaf flycheck-inline
        :hook (flycheck-mode-hook . flycheck-inline-mode)))

(defun toy/init-rustic ()
    (interactive)
    (visual-line-mode)
    (setq fill-column 100)
    (turn-on-auto-fill))


;; (leaf cargo
;;     :hook (rust-mode-hook . cargo-minor-mode))

;; (progn ;; TODO: C#
;;     (leaf csharp-mode ;;         )
;;     (leaf omnisharp
;;         ;; https://github.com/OmniSharp/omnisharp-emacs#:~:text=omnisharp-emacs%20is%20a%20port,that%20works%20in%20the%20background.
;;         )
;;     )

(progn ;; zig
    ;; (flycheck-define-checker zig
    ;;     "A zig syntax checker using the zig-fmt interpreter."
    ;;     :command ("zig" "fmt" (eval (buffer-file-name)))
    ;;     :error-patterns
    ;;     ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
    ;;     :modes zig-mode)
    ;; (add-to-list 'flycheck-checkers 'zig)

    ;; ;; ZLS: https://github.com/zigtools/zls
    ;; (with-eval-after-load 'lsp-mode
    ;;     (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
    ;;     (lsp-register-client (make-lsp-client
    ;;                           :new-connection (lsp-stdio-connection lsp-zig-zls-executable)
    ;;                           :major-modes '(zig-mode)
    ;;                           :server-id 'zls))
    ;;     )
    )

(leaf ccls ;; C, C++
    :hook (c-mode-hook . lsp-deferred)
    :hook (c++-mode-hook . lsp-deferred)
    :config
    ;; FIXME: this is mac-only
    (setq ccls-executable "/usr/local/bin/ccls"))

(leaf go-mode
    :config
    (add-hook 'go-mode-hook
              ;; FIXME: it would work even if it's not in `go-mode`
              (_fn (add-hook 'before-save-hook #'lsp-format-buffer t t)
                   (add-hook 'before-save-hook #'lsp-organize-imports t t)
                   (lsp-mode)
                   (lsp-ui-mode)
                   (flycheck-mode))))

;; TODO: idirs2-mode?
(leaf idris-mode
    :mode ("\\.l?idr\\'" . idris-mode)
    :hook (idirs-mode-hook . lsp-deferred)
    :config
    (add-to-list 'lsp-language-id-configuration '(idris-mode . "idris2"))
    (setq idris-interpreter-path "~/.idris2/bin/idris2")

    ;; (with-eval-after-load 'lsp-mode ;; LSP support for tlp-mode
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "idris2-lsp")
      :major-modes '(idris-mode)
      :server-id 'idris2-lsp))
    ;; )
    )

(leaf slime
    :config
    (setq inferior-lisp-program "sbcl"))

;; (leaf html-ls)

(leaf tide
    :after (typescript-mode company flycheck)
    :hook ((typescript-mode-hook . tide-setup)
           (typescript-mode-hook . tide-hl-identifier-mode)
           (before-save-hook . tide-format-before-save)))

;; ------------------------------ Markup languages ------------------------------

(with-eval-after-load 'evil 
(evil-define-key 'normal outline-minor-mode-map
    "z1" (_fn (outline-hide-sublevels 3))
    "z2" (_fn (outline-hide-sublevels 4))
    "z3" (_fn (outline-hide-sublevels 5))
    "z4" (_fn (outline-hide-sublevels 6))
    "z5" (_fn (outline-hide-sublevels 7))
    "z6" (_fn (outline-hide-sublevels 8))
    "z9" (_fn (outline-hide-sublevels 11))
    "z0" #'evil-open-folds
    ))

