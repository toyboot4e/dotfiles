;; Intelligence and UI

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

;; Set up `PATH` and `exec-path`
(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/opt/local/bin" "/sw/bin"
                   "~/.cargo/bin" "/usr/local/bin"
                   "~/bin"
                   ;; Unforunate path to LaTeX on my mac
                   "/Library/TeX/texbin"
                   "/usr/local/texlive/2019/bin/x86_64-darwin/"
                   ))

    (when (and (file-exists-p dir) (not (member dir exec-path)))
        (setenv "PATH" (concat dir ":" (getenv "PATH")))
        (setq exec-path (append (list dir) exec-path))))

;; ------------------------------ Widgets ------------------------------

;; some dependency needs it?
(use-package popup)

;; Zoom in to a pane: https://github.com/emacsorphanage/zoom-window
(use-package zoom-window
    :commands (darkroom-mode))

;; Zen mode *per buffer* (not per frame and that is great!)
(use-package olivetti
    ;; https://github.com/rnkn/olivetti
    :commands (olivetti-mode)
    :custom
    (olivetti-body-width 100))

;; Zen mode per buffer?
(use-package darkroom
    ;; https://github.com/joaotavora/darkroom
    :commands (darkroom-mode))

;; Zen mode per frame?
(use-package writeroom-mode
    ;; https://github.com/joostkremers/writeroom-mode
    :commands (writeroom-mode))

(use-package magit
    ;; https://github.com/magit/magit
    :commands (magit)

    ;; NOTE: use Zen mode in `magit-status`
    ;; :hook (magit-status-mode  . olivetti-mode)
    :config
    (evil-define-key 'normal 'magit-mode-map
        "zz" #'recenter-top-bottom
        "z-" #'evil-scroll-line-to-bottom
        "zb" #'evil-scroll-line-to-bottom
        (kbd "z RET") #'evil-scroll-line-to-top
        "zt" #'evil-scroll-line-to-top
        )

    ;; show line numbers (NOT the line numbers of the corresponding source)
    ;; (setq magit-disable-line-numbers t)
    ;; (setq magit-section-disable-line-numbers t)
    )

;; add TODO list to magit
(use-package magit-todos
    ;; https://github.com/alphapapa/magit-todos
    :commands (magit-todos-list)
    :after magit)

;; ;; not working well
;; (use-package magit-delta
;;     ;; https://github.com/dandavison/magit-delta
;;     :commands magit-delta-mode
;;     :hook (magit-status-mode . magit-delta-mode)
;;     :init
;;     (setq magit-delta-delta-args
;;           '("--24-bit-color" "always"
;;             "--features" "magit-delta"
;;             "--color-only"))
;;     )

;; c.f. https://magit.vc/manual/forge/
(use-package forge
    :after magit
    )

(use-package centaur-tabs
    ;; https://github.com/ema2159/centaur-tabs
    ;; NOTE: it adds default bindings go to `C-c C-c`
    :config
    ;; show buffers in current group
    (setq centaur-tabs--buffer-show-groups nil)

    ;; navigate through buffers only in the current group
    (setq centaur-tabs-cycle-scope 'tabs)

    ;; show underline for the current buffer
    (setq centaur-tabs-set-bar 'under
          x-underline-at-descent-line t)

    ;; configure
    (setq centaur-tabs-style "bar"
          centaur-tabs-height 24
          centaur-tabs-set-modified-marker t
          centaur-tabs-gray-out-icons 'buffer ;; gray out unfocused tabs
          centaur-tabs-show-navigation-buttons nil
          centaur-tabs-set-icons (display-graphic-p)
          )
    (centaur-tabs-group-by-projectile-project)

    (centaur-tabs-headline-match)
    (centaur-tabs-mode t))

(use-package neotree
    ;; https://github.com/jaypei/emacs-neotree
    ;; NOTE: when renaming, press `TAB` to not select Ivy completion item
    :after evil
    :commands (neotree-quick-look)
    :init
    (setq neo-theme (if (display-graphic-p) 'icons 'arrows)
          neo-window-position 'right  ;; show on right side
          neo-window-width 25         ;; preferred with
          neo-window-fixed-size nil   ;; resize with mouse
          neo-show-hidden-files t     ;; show hidden files
          )

    :config
    ;; [GUI] set fontsize
    (when (display-graphic-p)
        (add-hook 'neo-after-create-hook
                  (lambda (_)
                      (text-scale-adjust 0)
                      (text-scale-decrease 0.5) ;; the bigger, the smaller
                      )))

    (evil-define-key 'normal neotree-mode-map
        (kbd "RET") #'neotree-enter

        ;; open
        "oo" #'neotree-enter
        "ov" #'neotree-enter-vertical-split
        "oh" #'neotree-enter-horizontal-split

        ;; change directory
        "cd" #'neotree-change-root    ;; cd: chande directory
        "cu" #'neotree-select-up-node ;; cu: up directory
        "cc" #'neotree-copy-node      ;; cc: copy

        ;; menu (file/directory operation)
        "mc" #'neotree-create-node    ;; mc: create node
        "md" #'neotree-delete-node    ;; md: delete node
        "mr" #'neotree-rename-node    ;; mr: rename mode

        ;; view
        "h" #'neotree-hidden-file-toggle
        "r" #'neotree-refresh

        ;; else
        "q" #'neotree-hide          ;; quit
        ;; prefer `zz`
        ;; "z" #'neotree-stretch-toggle
        (kbd "TAB") 'neotree-stretch-toggle
        )
    )

;; ------------------------------ GitHub ------------------------------

(use-package git-link
    :commands (git-link git-link-commit)
    :config
    (defun git-link-open-ln ()
        (interactive)
        (let ((git-link-open-in-browser t))
            (call-interactively #'git-link)))

    (defun git-link-today-ln ()
        (interactive)
        (let ((git-link-use-commit t))
            (call-interactively #'git-link)))

    (defun git-link-open-today-ln ()
        (interactive)
        (let ((git-link-use-commit t)
              (git-link-open-in-browser t))
            (call-interactively #'git-link)))

    ;; no line number
    (defun git-link-open ()
        (interactive)
        (let ((git-link-use-single-line-number t))
            (git-link-open-ln)))

    (defun git-link-open-today ()
        (interactive)
        (let ((git-link-use-single-line-number t))
            (git-link-today-ln)))

    (defun git-link-open-today ()
        (interactive)
        (let ((git-link-use-single-line-number t))
            (git-link-open-today-ln)))

    )

;; ------------------------------ Workspace ------------------------------

(tab-bar-mode 1)

;; use project name as default tab name
(defun toy/set-tab-name-default ()
    (let ((proj-name (projectile-project-name)))
        (when proj-name (tab-bar-rename-tab proj-name))))

(advice-add 'tab-bar-new-tab :after (lambda (&rest x) (toy/set-tab-name-default)))
(add-hook 'window-setup-hook #'toy/set-tab-name-default)

;; ------------------------------ Shell ------------------------------

;; TODO: shell
(use-package vterm
    :config
    (defun toy/turn-off-chrome ()
        (hl-line-mode -1)
        (display-line-numbers-mode -1))
    :hook (vterm-mode . toy/turn-off-chrome))

;; (use-package vterm-toggle
;;                                         ;j    :custom
;;     (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
;;     (vterm-toggle-scope 'projectile)
;;     ;; :bind (("C-c t" . #'vterm-toggle)
;;     ;;        :map vterm-mode-map
;;     ;;        ("s-t" . #'vterm) ; Open up new tabs quickly
;;     ;;        )
;;     )

;; ------------------------------ Intelligence ------------------------------

(use-package company ;; COMPlete ANYthing
    ;; http://company-mode.github.io/
    :hook (prog-mode . company-mode)
    ;; :bind is done by `evil-collection`
    :init
    (setq company-idle-delay 0             ;; default: `0.2`
          company-minimum-prefix-length 1
          company-selection-wrap-around t
          ))

(use-package company-box
    ;; show icons in the completion menu
    ;; https://github.com/sebastiencs/company-box
    :if (display-graphic-p)
    :hook (company-mode . company-box-mode))

(use-package projectile
    ;; https://github.com/bbatsov/projectile
    :init
    (setq projectile-enable-caching t)
    :config (projectile-mode +1))

(use-package hydra
    ;; https://github.com/abo-abo/hydra
    :defer t)

;; ------------------------------ LSP ------------------------------

;; this is recommended by `lsp-mode`
(use-package flycheck :defer t)

(use-package lsp-mode
    ;; https://emacs-lsp.github.io/lsp-mode
    ;; NOTE: Don't use `lsp`, where `lsp-ui-mode` is not hooked
    :commands (lsp-mode lsp-deferred)
    :hook (lsp-mode . lsp-enable-which-key-integration)

    :init
    ;; NOTE: manual binding to `lsp-commad-map` is below (`define-key`)
    (setq lsp-keymap-prefix nil)

    (setq lsp-log-io t             ; output log to `*lsp-log*`
          lsp-trace t
          lsp-print-performance t  ; also performance information
          )

    :config
    ;; hide: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
    (setq lsp-eldoc-enable-hover nil
          lsp-signature-auto-activate nil
          lsp-signature-render-documentation nil
          lsp-completion-show-kind nil
          lsp-enable-symbol-highlighting nil
          lsp-headerline-breadcrumb-enable nil
          )
    (setq lsp-modeline-diagnostics-scope :workspace)

    (define-key evil-normal-state-map " l" lsp-command-map)
    (evil-define-key 'normal lsp-mode-map
        "K" #'lsp-describe-thing-at-point
        )
    )

;; NOTE: `lsp-ui` can be too slow (especially on Windows). Consider disabling it then.
(use-package lsp-ui
    ;; https://emacs-lsp.github.io/lsp-ui/
    ;; automatically activated by `lsp-mode` automatically
    :commands lsp-ui-mode
    :hook (lsp-mode . lsp-ui-mode)

    :bind (:map lsp-command-map
                ("i" . lsp-ui-imenu)
                ;; `ge`: `lsp-treemacs-error-list` (default)
                ("gf" . lsp-ui-flycheck-list)

                )

    :config
    (setq lsp-idle-delay 0.500
          lsp-ui-sideline-delay 0
          lsp-ui-doc-delay 0)

    ;; hide
    (setq lsp-ui-doc-enable nil
          lsp-ui-doc-position 'top
          )

    ;; show errors on sideline
    (setq lsp-ui-sideline-show-diagnostics t
          lsp-ui-sideline-show-hover nil
          lsp-ui-sideline-show-code-actions nil
          )

    ;; `lsp-ui-imenu` is awesome!
    (setq lsp-ui-imenu-window-width 30)

    (evil-define-key 'normal lsp-ui-imenu-mode-map
        ;; preview
        (kbd "TAB") #'lsp-ui-imenu--view
        ;; go
        (kbd "RET") #'lsp-ui-imenu--visit
        )
    )

;; (use-package imenu-list
;;     :bind
;;     ("<f10>" . imenu-list-smart-toggle)
;;     ;; :custom-face
;;     ;; (imenu-list-entry-face-1 ((t (:foreground "white"))))
;;     :custom
;;     (imenu-list-focus-after-activation t)
;;     (imenu-list-auto-resize nil)
;;     (imenu-list-size 0.15)
;;     )

;; TODO: fix error somehow
;; (use-package lsp-treemacs
;;     ;; https://github.com/emacs-lsp/lsp-treemacs
;;     ;; Default key mappings:
;;     ;; * `lsp-error-list`: `<lsp-keymap-prefix> g e`
;;     :after lsp-mode
;;     :config
;;     ;; TODO: set up key mappings
;;     (evil-set-initial-state 'lsp-treemacs-error-list-mode
;;                             'emacs)
;;     (lsp-treemacs-sync-mode 1))

