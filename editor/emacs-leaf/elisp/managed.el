;;; managed.el --- DB of packages managed by `leaf-manager'  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 toyboot4e

;; Author: toyboot4e <toyboot4e@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:



;;; Code:

(prog1 'emacs)

(leaf leaf-manager
    :custom `((leaf-manager-file \,
                                 (concat user-emacs-directory "elisp/managed.el"))
              (leaf-manager-template-feature-name . "managed")
              (leaf-manager-template-summary . "DB of packages managed by `leaf-manager'")
              (leaf-manager-template-commentary . "")
              (leaf-manager-template-local-variables . ";; fill-column: 100"))
    :config
    (leaf adoc-mode
        :mode ("\\.adoc\\'" . adoc-mode)
        :config
        (defun toy/init-adoc-mode nil
            (interactive)
            (outline-minor-mode)
            (setq-local electric-indent-mode nil))

        (add-hook 'LaTeX-mode-hook
                  (lambda nil
                      (electric-indent-local-mode -1)))
        :hook toy/init-adoc-mode)

    (leaf aggressive-indent
        :hook (emacs-lisp-mode-hook scheme-mode-hook))

    (leaf all-the-icons
        :if (display-graphic-p))

    (leaf auto-package-update
        :init
        (setq auto-package-update-last-update-day-filename (concat user-emacs-directory "tmp/.last-package-update-day"))
        :custom ((auto-package-update-delete-old-versions . t)
                 (auto-package-update-interval . 7))
        :config
        (auto-package-update-maybe))

    (leaf blamer
        :straight (blamer :type git :host github :repo "Artawower/blamer.el")
        :custom ((blamer-idle-time . 0.3)
                 (blamer-min-offset . 70))
        :custom-face (blamer-face \`
                                  ((t :foreground "#7a88cf" :background nil :height 140 :italic t))))

    (leaf centaur-tabs
        :url "https://github.com/ema2159/centaur-tabs"
        :after projectile
        :custom ((centaur-tabs--buffer-show-groups)
                 (centaur-tabs-cycle-scope quote tabs)
                 (centaur-tabs-set-bar quote under)
                 (x-underline-at-descent-line . t)
                 (centaur-tabs-style . "bar")
                 (centaur-tabs-height . 24)
                 (centaur-tabs-set-modified-marker . t)
                 (centaur-tabs-gray-out-icons quote buffer)
                 (centaur-tabs-show-navigation-buttons)
                 (centaur-tabs-set-icons display-graphic-p))
        :custom (centaur-tabs-buffer-groups-function function toy/centaur-tabs-group)
        :config
        (defun toy/centaur-tabs-group nil
            "Add `Sidebar' and `Bottom bar' groups / use `projectile' buffer gruups"
            (cond
             ((string-equal "@"
                            (substring
                             (buffer-name)
                             0 1))
              '("Sidebar"))
             ((string-equal "⊥"
                            (substring
                             (buffer-name)
                             0 1))
              '("Bottom bar"))
             ((or (string-equal "COMMIT-EDITMSG" (buffer-name))
                  (and (> (length (buffer-name)) 5)
                       (string-equal "magit"
                                     (substring
                                      (buffer-name)
                                      0 5))))
              '("magit"))
             (t
              (centaur-tabs-projectile-buffer-groups))))

        (centaur-tabs-mode t)
        :defer-config (centaur-tabs-headline-match))

    ;; Didn't woring working on `tmux' int 'kitty'
    ;; (leaf clipetty
    ;;     :after evil
    ;;     :config
    ;;     (evil-ex-define-cmd "copy" #'clipetty-kill-ring-save))
    ;;
    ;; (leaf simpleclip-mode
    ;;     :ensure nil
    ;;     :straight (simpleclip-mode :type git :host github :repo "rolandwalker/simpleclip")
    ;;     :config (simpleclip-mode 1))

    (leaf xclip
        :config (xclip-mode))

    (leaf cmake-mode)

    (leaf dhall-mode
        :mode "\\.dhall\\'"
        :hook (dhall-mode-hook . lsp-deferred)
        :hook (dhall-mode-hook . lsp-ui-mode)
        :custom ((dhall-use-header-line)
                 (dhall-format-arguments
                  `("--ascii"))))

    (leaf dirvish
        :doc "A modern file manager based on dired mode"
        :req "emacs-27.1"
        :url "https://github.com/alexluigit/dirvish"
        :emacs>= 27.1)

    (leaf doom-modeline
        :url "https://github.com/seagle0128/doom-modeline"
        :leaf-defer nil
        :custom ((doom-modeline-icon display-graphic-p)
                 (doom-modeline-major-mode-icon display-graphic-p)
                 (doom-modeline-height . 18)
                 (doom-modeline-buffer-encoding)
                 (doom-modeline-buffer-file-name-style quote truncate-upto-project))
        :config
        (advice-add 'vc-git-mode-line-string :filter-return
                    (lambda (arg)
                        (substring arg 4)))
        (leaf minions
            :doc "Hide minor mode names in the [+] tab (no need for `diminish'!)"
            :custom ((minions-mode-line-lighter . "[+]")
                     (doom-modeline-minor-modes . t))
            :config
            (minions-mode 1))

        :defer-config (doom-modeline-mode))

    (leaf editorconfig
        :config
        (editorconfig-mode 1))

    (leaf eldoc
        :ensure nil
        :tag "builtin")

    (leaf evil
        :custom ((evil-toggle-key . "")
                 (evil-want-keybinding)
                 (evil-want-C-u-delete . t)
                 (evil-want-C-u-scroll . t)
                 (evil-want-Y-yank-to-eol . t)
                 (evil-move-cursor-back \.t)
                 (evil-search-module quote evil-search))
        :config
        (evil-mode 1)
        (progn
            (evil-ex-define-cmd "ed"
                                (lambda nil
                                    (interactive)
                                    (evil-edit
                                     (concat user-emacs-directory "init.el"))))
            (evil-ex-define-cmd "s" #'toy/reload)
            (evil-ex-define-cmd "Bd" #'kill-this-buffer)
            (evil-ex-define-cmd "BD" #'kill-this-buffer)
            (evil-ex-define-cmd "hs" #'evil-window-split))

        (leaf empv
            :ensure nil
            :straight (empv :type git :host github :repo "isamert/empv.el"))

        (leaf undo-tree
            :custom (undo-tree-auto-save-history)
            :init
            (evil-set-undo-system 'undo-tree)
            (global-undo-tree-mode))

        (leaf evil-anzu
            :url "https://github.com/emacsorphanage/evil-anzu"
            :commands "anzu-query-replace-regexp")

        (leaf evil-surround
            :config
            (global-evil-surround-mode))

        (leaf expand-region
            :after evil
            :config
            (evil-define-key 'visual 'global "v" #'er/expand-region "V" #'er/contract-region)))

    (leaf evil-collection
        :after evil
        :leaf-defer nil
        :commands (evil-collection-dired-setup evil-collection-elfeed-setup evil-collection-elisp-mode-setup evil-collection-eww-setup evil-collection-info-setup evil-collection-markdown-mode-setup evil-collection-consult-setup evil-collection-corfu-setup evil-collection-embark-setup evil-collection-vterm-setup evil-collection-magit-setup evil-collection-pdf-setup evil-collection-doc-view-setup evil-collection-minibuffer-mode-setup evil-collection-slime-setup evil-collection-sly-setup)
        :custom (evil-collection-magit-use-z-for-folds . t)
        :config
        (with-eval-after-load 'dired
            (evil-collection-dired-setup))

        (with-eval-after-load 'elfeed
            (evil-collection-elfeed-setup))

        (with-eval-after-load 'elisp-mode
            (evil-collection-elisp-mode-setup))

        (with-eval-after-load 'eww
            (evil-collection-eww-setup))

        (with-eval-after-load 'info
            (evil-collection-info-setup))

        (with-eval-after-load 'markdown-mode
            (evil-collection-markdown-mode-setup))

        (with-eval-after-load 'minibuffer-mode
            (evil-collection-minibuffer-mode-setup))

        (with-eval-after-load 'consult
            (evil-collection-consult-setup))

        (with-eval-after-load 'corfu
            (evil-collection-corfu-setup))

        (with-eval-after-load 'embark
            (evil-collection-embark-setup))

        (with-eval-after-load 'forge
            (evil-collection-forge-setup))

        (with-eval-after-load 'doc-view
            (evil-collection-doc-view-setup))

        (with-eval-after-load 'org-roam
            (evil-collection-org-roam-setup))

        (with-eval-after-load 'pdf
            (evil-collection-pdf-setup))

        (with-eval-after-load 'vterm
            (evil-collection-vterm-setup))

        (with-eval-after-load 'slime
            (evil-collection-slime-setup))

        (with-eval-after-load 'sly
            (evil-collection-sly-setup))

        (with-eval-after-load 'magit
            (evil-define-key 'normal magit-mode-map "zz" #'evil-scroll-line-to-center "z-" #'evil-scroll-line-to-bottom
                (kbd "z RET")
                #'evil-scroll-line-to-top
                (kbd "SPC RET")
                #'magit-diff-visit-worktree-file-other-window)
            (evil-define-key 'normal git-rebase-mode-map "C-j" git-rebase-move-line-down "C-u" git-rebase-move-line-up)
            (advice-add 'magit-section-forward :after
                        (lambda (&rest _)
                            (evil-scroll-line-to-top
                             (line-number-at-pos))))
            (advice-add 'magit-section-backward :after
                        (lambda (&rest _)
                            (evil-scroll-line-to-top
                             (line-number-at-pos))))
            (advice-add 'magit-section-forward-sibling :after
                        (lambda (&rest _)
                            (evil-scroll-line-to-top
                             (line-number-at-pos))))
            (advice-add 'magit-section-backward-sibling :after
                        (lambda (&rest _)
                            (evil-scroll-line-to-top
                             (line-number-at-pos))))
            (evil-collection-magit-setup)))

    (leaf evil-escape
        :doc "Smart escape with `jk` or `kj`"
        :custom ((evil-escape-key-sequence . "jk")
                 (evil-escape-unordered-key-sequenceu . t))
        :config
        (evil-escape-mode))

    (leaf evil-exchange
        :doc "Use `gx` to swap"
        :url "https://github.com/Dewdrops/evil-exchange"
        :config
        (evil-exchange-install))

    (leaf evil-lion
        :doc "Add `gl` and `gL` algin operators"
        :url "https://github.com/edkolev/evil-lion"
        :after evil
        :config
        (evil-define-key 'normal 'toy/global-mode-map "gl" #'evil-lion-left "gL" #'evil-lion-right)
        (evil-define-key 'visual 'toy/global-mode-map "gl" #'evil-lion-left "gL" #'evil-lion-right)
        (evil-lion-mode))

    (leaf evil-matchit
        :doc "Smarter `%` motion"
        :config
        (global-evil-matchit-mode 1))

    (leaf evil-nerd-commenter
        :doc "Toggle comment"
        :commands (evilnc-comment-or-uncomment-lines))

    (leaf evil-org
        :after evil org
        :hook (org-mode-hook . evil-org-mode)
        :hook (evil-org-mode-hook . toy/init-evil-org)
        :config
        (defun toy/init-evil-org nil
            (interactive)
            (evil-org-set-key-theme)
            (evil-define-key 'motion 'evil-org-mode "d" 'evil-delete)))

    (leaf evil-string-inflection
        :doc "Add `g~` operator to cycle through string cases"
        :url "https://github.com/ninrod/evil-string-inflection")

    (leaf fill-column-indicator
        :doc "Graphically indicate the fill column"
        :if nil
        :custom (fci-rule-color . "#c0b18b")
        :config
        (setq-default fill-column 100)
        (define-globalized-minor-mode global-fci-mode fci-mode
            (lambda nil
                (fci-mode 1)))
        (global-fci-mode 1))

    (leaf fish-mode)

    (leaf flycheck)

    (leaf git-gutter
        :doc "Show git status on line numbers (terminal)"
        :if (not (display-graphic-p))
        :global-minor-mode (global-git-gutter-mode)
        :custom ((git-gutter:modified-sign . "~")
                 (git-gutter:added-sign . "+")
                 (git-gutter:deleted-sign . "-"))
        :custom-face (git-gutter:modified quote
                                          ((t
                                            (:background "#c0b18b" :foreground "#2f2f2f")))) (git-gutter:added quote
                                          ((t
                                            (:background "#84edb9" :foreground "#2f2f2f")))) (git-gutter:deleted quote
                                          ((t
                                            (:background "#d75f5f" :foreground "#2f2f2f")))))

    (leaf git-gutter-fringe
        :doc "Show git status on line numbers (GUI)"
        :if (display-graphic-p)
        :global-minor-mode (global-git-gutter-mode)
        :custom ((git-gutter-fr:modified-sign . "~")
                 (git-gutter-fr:added-sign . "+")
                 (git-gutter-fr:deleted-sign . "-"))
        :custom-face (git-gutter-fr:modified quote
                                             ((t
                                               (:background "#c0b18b" :foreground "#2f2f2f")))) (git-gutter-fr:added quote
                                             ((t
                                               (:background "#84edb9" :foreground "#2f2f2f")))) (git-gutter-fr:deleted quote
                                             ((t
                                               (:background "#d75f5f" :foreground "#2f2f2f")))))

    (leaf git-link
        :commands (git-link git-link-commit)
        :commands (gl-line gl-today)
        :config
        (defun gl-line nil
            (interactive)
            (let ((git-link-use-commit t))
                (ignore git-link-use-commit)
                (call-interactively #'git-link)))

        (defun gl-today nil
            (interactive)
            (let ((git-link-use-commit t)
                  (git-link-open-in-browser t))
                (ignore git-link-use-commit git-link-open-in-browser)
                (call-interactively #'git-link))))

    (leaf git-modes)

    (leaf glsl-mode
        :mode (("\\.fs" . glsl-mode)
               ("\\.vs" . glsl-mode)
               ("\\.glsl" . glsl-mode)
               ("\\.frag" . glsl-mode)
               ("\\.vert" . glsl-mode)))

    (leaf gnuplot-mode
        :mode (("\\.gp\\'" . gnuplot-mode)))

    (leaf google-translate
        :doc "Emacs interface to Google Translate."
        :url "https://github.com/atykhonov/google-translate")

    (leaf haskell-mode
        :url "https://github.com/haskell/haskell-mode"
        :hook (haskell-mode-hook . lsp-deferred)
        :hook (haskell-literate-mode-hook . lsp-deferred)
        :config
        (defun ormolu-format-buffer ()
            "Formats current buffer with `ormolu'.
Thanks: `https://www.masteringemacs.org/article/executing-shell-commands-emacs'"
            (interactive)
            (setq last-point (point))
            (shell-command-on-region
             (point-min) (point-max)
             (format "ormolu --stdin-input-file %s" (buffer-file-name))
             ;; output buffer, replace?, name of error buffer, show it
             (current-buffer) t
             "*Ormolu Error Buffer*" t)
            (goto-char last-point))

        (leaf consult-hoogle
            :ensure nil
            :straight (consult-hoogle :type git :host github :repo "aikrahguzar/consult-hoogle"))

        (leaf lsp-haskell
            :after lsp-mode
            :custom
            :url "https://github.com/emacs-lsp/lsp-haskell")

        (evil-define-key 'normal 'haskell-mode-map
            (kbd "C-c h") 'consult-hoogle
            (kbd "C-c f") 'ormolu-format-buffer)
        )

    (leaf helpful
        :bind ([remap describe-command]
               . helpful-command) ([remap describe-key]
               . helpful-key)
        :after evil
        :init
        (evil-define-key 'normal helpful-mode-map "q" #'kill-this-buffer)
        (evil-define-key 'normal 'global "K" #'helpful-at-point))

    (leaf hl-todo
        :doc "highlight TODO, FIXME, etc."
        :custom ((hl-todo-highlight-punctuation . ":")
                 (hl-todo-keyword-faces \`
                                        (("TODO" warning bold)
                                         ("FIXME" error bold)
                                         ("HACK" font-lock-constant-face bold)
                                         ("REVIEW" font-lock-keyword-face bold)
                                         ("NOTE" success bold)
                                         ("WIP" font-lock-keyword-face bold)
                                         ("REMARK" success bold)
                                         ("DEPRECATED" font-lock-doc-face bold))))
        :config
        (global-hl-todo-mode 1))

    (leaf hydra)

    (leaf lsp-mode
        :after evil
        :commands (lsp-mode lsp-deferred)
        :doc "`lsp-semantic-token-enable' is set to `nil' preferring `tree-sitter'"
        :hook (lsp-mode-hook . lsp-enable-which-key-integration)
        :hook (lsp-mode-hook . hs-minor-mode)
        :hook (c-mode-hook . lsp-deferred)
        :hook (cpp-mode-hook . lsp-deferred)
        :custom ((lsp-completion-provider . :none)
                 (lsp-completion-show-kind)
                 (lsp-keymap-prefix)
                 (lsp-idle-delay . 0.5)
                 (lsp-log-io)
                 (lsp-trace)
                 (lsp-print-performance)
                 (lsp-eldoc-enable-hover)
                 (lsp-signature-auto-activate)
                 (lsp-signature-render-documentation)
                 (lsp-enable-symbol-highlighting)
                 (lsp-headerline-breadcrumb-enable)
                 (lsp-modeline-diagnostics-scope . :workspace)
                 (lsp-semantic-tokens-enable))
        :preface
        :config
        (progn
            (defun toy/c-on-save nil
                (when (eq major-mode 'c-mode)
                    (lsp-format-buffer)))

            (add-hook 'before-save-hook #'toy/c-on-save)
            (defun toy/cpp-on-save nil
                (when (eq major-mode 'c++-mode)
                    (lsp-format-buffer)))

            (add-hook 'before-save-hook #'toy/cpp-on-save))

        :defer-config (define-key evil-normal-state-map " l" lsp-command-map) (evil-define-key 'normal lsp-mode-map "K" #'lsp-describe-thing-at-point))

    (leaf lsp-ui
        :commands lsp-ui-mode
        :hook (lsp-mode-hook . lsp-ui-mode)
        :after evil
        :custom ((lsp-idle-delay . 0.5)
                 (lsp-ui-sideline-delay . 0)
                 (lsp-ui-doc-delay . 0)
                 (lsp-ui-doc-enable)
                 (lsp-ui-doc-position quote top)
                 (lsp-ui-sideline-show-diagnostics . t)
                 (lsp-ui-sideline-show-hover)
                 (lsp-ui-sideline-show-code-actions)))

    (leaf lsp-ui-imenu
        :ensure nil
        :custom ((lsp-imenu-sort-methods quote
                                         (position))
                 (lsp-imenu-index-symbol-kinds quote
                                               (Class Method Proeprty Constructor Enum Interface Function Variable Constant String Number Boolean Array Object Key Struct Event Operator))
                 (lsp-ui-imenu-buffer-name . toy/sidebar-imenu-buffer-name)
                 (lsp-ui-imenu-window-width . toy/sidebar-width))
        :hook (lsp-ui-imenu-mode-hook . hl-line-mode)
        :custom-face (hl-line quote
                              ((t
                                (:background "#458588"))))
        :defer-config (evil-define-key 'normal lsp-ui-imenu-mode-map
                          (kbd "TAB")
                          #'lsp-ui-imenu--view
                          (kbd "RET")
                          #'lsp-ui-imenu--visit) (advice-add 'lsp-ui-imenu--visit :after
                          (lambda (&rest _)
                              (toy/force-center))))

    (leaf lua-mode)

    (leaf macrostep
        :doc "interactive macro expander"
        :config
        (define-key emacs-lisp-mode-map
            (kbd "C-c e")
            'macrostep-expand))

    (leaf magit
        :url "https://github.com/magit/magit"
        :commands (magit)
        :after evil
        :custom (magit-log-section-commit-count . 40)
        :config
        (defun magit-rev-format (format &optional rev args)
            "lighter magit revision format"
            (let ((str (magit-git-string "log" "-1" "--no-patch"
                                         (concat "--format=" format)
                                         args
                                         (if rev
                                                 (concat rev "^{commit}")
                                             "HEAD")
                                         "--")))
                (unless (string-equal str "")
                    str)))

        (setq-default transient-history-file
                      (concat user-emacs-directory "tmp/transient/history.el")
                      transient-values-file
                      (concat user-emacs-directory "tmp/transient/values.el")
                      transient-levels-file
                      (concat user-emacs-directory "tmp/transient/levels.el"))
        (evil-define-key 'normal 'magit-mode-map "zz" #'recenter-top-bottom "z-" #'evil-scroll-line-to-bottom "zb" #'evil-scroll-line-to-bottom
            (kbd "z RET")
            #'evil-scroll-line-to-top "zt" #'evil-scroll-line-to-top)
        (leaf magit-todos
            :commands (magit-todos-list)
            :after magit)

        (leaf forge
            :doc "Use GitHub on Emacs"))

    (leaf markdown-mode
        :commands (markdown-mode gfm-mode)
        :mode (("README\\.md\\'" . gfm-mode)
               ("\\.md\\'" . markdown-mode)
               ("\\.markdown\\'" . markdown-mode))
        :after evil
        :custom (markdown-command . "multimarkdown")
        :config
        (evil-define-key 'normal markdown-mode-map "z1"
            (_fn
             (outline-hide-sublevels 1))
            "z2"
            (_fn
             (outline-hide-sublevels 2))
            "z3"
            (_fn
             (outline-hide-sublevels 3))
            "z4"
            (_fn
             (outline-hide-sublevels 4))
            "z5"
            (_fn
             (outline-hide-sublevels 5))
            "z6"
            (_fn
             (outline-hide-sublevels 6))
            "z9"
            (_fn
             (outline-hide-sublevels 9))
            "z0" #'evil-open-folds))

    (leaf neotree
        :url "https://github.com/jaypei/emacs-neotree"
        :after evil
        :commands (neotree-quick-look)
        :init
        (setq neo-theme (if (display-graphic-p)
                                'icons 'arrows))
        :custom ((neo-window-position quote right)
                 (neo-window-width . toy/sidebar-width)
                 (neo-window-fixed-size)
                 (neo-show-hidden-files . t))
        :config
        (setq neo-buffer-name "@tree")
        (when (display-graphic-p)
            (add-hook 'neo-after-create-hook
                      (lambda (_)
                          (text-scale-adjust 0)
                          (text-scale-decrease 0.5))))
        (evil-define-key 'normal neotree-mode-map "gh" #'neotree-select-up-node "oo" #'neotree-enter
            (kbd "RET")
            #'neotree-enter "ov" #'neotree-enter-vertical-split "oh" #'neotree-enter-horizontal-split "cd" #'neotree-change-root "cu" #'neotree-select-up-node "cc" #'neotree-copy-node "mc" #'neotree-create-node "md" #'neotree-delete-node "mr" #'neotree-rename-node "h" #'neotree-hidden-file-toggle "r" #'neotree-refresh "q" #'neotree-hide
            (kbd "TAB")
            'neotree-stretch-toggle)
        :defer-config (defun neo-path--shorten (path length)
                          "Override `neotree' header string"
                          (file-name-nondirectory
                           (directory-file-name path)))
        (advice-add 'neotree-select-up-node :after
                    (lambda (&rest _)
                        (evil-first-non-blank))))

    (leaf nix-mode
        ;; :mode "\\.nix\\'"
        :hook (nix-mode-hook . lsp-deferred)
        :config
        (leaf lsp-nix
            :custom
            (lsp-nix-nil-formatter . ["nixpkgs-fmt"])))

    (leaf olivetti
        :doc "Zen mode *per buffer* (not per frame and that is great!)"
        :url "https://github.com/rnkn/olivetti"
        :commands (olivetti-mode)
        :custom (olivetti-body-width . 120))

    ;; FIXME: not workings
    ;; TODO: hook mode
    (leaf pdf-tools
        :init
        (defun toy/on-pdf-view ()
            ;; (leaf org-pdfview)
            (pdf-view-mode)
            (pdf-tools-enable-minor-modes)
            ;; TODO: run with timer
            (run-with-timer 1 nil #'pdf-view-fit-page-to-window)
            (pdf-outline-imenu-enable)
            )
        :config
        :hook  (doc-view-mode-hook . toy/on-pdf-view))

    (leaf projectile
        :leaf-defer nil
        :custom (projectile-enable-caching . t)
        :init
        (setq projectile-cache-file (concat user-emacs-directory "tmp/projectile.cache")
              projectile-known-projects-file (concat user-emacs-directory "tmp/projectile-bookmarks.eld"))
        :config
        (projectile-mode 1))

    (leaf racket-mode
        :doc "https://github.com/jeapostrophe/racket-langserver"
        :url "https://www.racket-mode.com/"
        :config
        (with-eval-after-load 'lsp-mode
            (add-to-list 'lsp-language-id-configuration
                         '(racket-mode . "racket"))
            (lsp-register-client
             (make-lsp-client :new-connection
                              (lsp-stdio-connection
                               '("racket" "-l" "racket-langserver"))
                              :activation-fn
                              (lsp-activate-on "racket")
                              :server-id 'racket-langserver))))

    (leaf rainbow-delimiters
        :config
        (define-globalized-minor-mode toy/global-rainbow-delimiters-mode rainbow-delimiters-mode
            (lambda nil
                (rainbow-delimiters-mode 1)))
        (toy/global-rainbow-delimiters-mode 1))

    (leaf rainbow-mode
        :doc "show color codes like this: #c0b18b"
        :config
        (define-globalized-minor-mode toy/global-rainbow-mode rainbow-mode
            (lambda nil
                (rainbow-mode 1)))
        (toy/global-rainbow-mode 1))

    (leaf ron-mode
        :mode (("\\.ron\\'" . ron-mode))
        :hook (ron-mode-hook lambda nil
                             (setq comment-start "// "
                                   comment-end "")))

    (leaf rust-mode
        :hook (rust-mode-hook . lsp-deferred)
        :hook (rust-mode-hook . toy/on-rust-mode)
        :mode ("\\.rs\\'" . rust-mode)

        :custom ((rust-load-optional-libraries . t)
                 (rust-format-on-save . t)
                 (rust-format-show-buffer)
                 (lsp-rust-analyzer-server-display-inlay-hints))

        :init
        ;; (defun rust-after-save-method ())
        (defun toy/on-rust-mode nil
            (interactive)
            (visual-line-mode)
            (setq fill-column 100)
            (turn-on-auto-fill))

        ;; (defun toy/on-save-rust ()
        ;;     (lsp-format-buffer)
        ;;     (centaur-tabs-on-saving-buffer)
        ;; :config
        ;; (add-hook 'after-save-hook #'toy/on-save-rust)
        )

    (leaf scratch-comment
        :bind ((lisp-interaction-mode-map
                :package elisp-mode
                ("C-j" . scratch-comment-eval-sexp))))
    (leaf popup)

    (leaf sml-mode
        :url "http://elpa.gnu.org/packages/sml-mode.html"
        :mode ("\\.ml\\'" . sml-mode)
        :mode ("\\.mli\\'" . sml-mode))

    (leaf vimish-fold
        :after evil
        :config
        (leaf evil-vimish-fold
            :custom ((evil-vimish-fold-mode-lighter . " ⮒")
                     (evil-vimish-fold-target-modes quote
                                                    (prog-mode conf-mode text-mode)))
            :config
            (global-evil-vimish-fold-mode)))

    (leaf vimrc-mode
        :mode ("\\.vim" . vimrc-mode)
        :mode ("\\.nvim" . vimrc-mode))

    (leaf wc-mode)

    (leaf wgsl-mode
        :doc "cargo install --git https://github.com/wgsl-analyzer/wgsl-analyzer wgsl_analyzer"
        :ensure nil
        :straight (wgsl-mode :type git :host github :repo "KeenS/wgsl-mode.el")
        :hook (wgsl-mode-hook . lsp-deferred)
        :hook (wgsl-mode-hook . lsp-ui-mode)
        :config
        (with-eval-after-load 'lsp-mode
            (add-to-list 'lsp-language-id-configuration
                         '(wgsl-mode . "wgsl"))
            (lsp-register-client
             (make-lsp-client :new-connection
                              (lsp-stdio-connection "~/.cargo/bin/wgsl_analyzer")
                              :major-modes
                              '(wgsl-mode)
                              :server-id 'wgsl))))

    (leaf which-key
        :custom ((which-key-idle-delay . 0.01)
                 (which-key-idle-secondary-delay . 0.01))
        :config
        (define-key help-map
            (kbd "M")
            'which-key-show-major-mode)
        (which-key-mode))

    (leaf yaml-mode)

    (leaf zig-mode
        :mode ("\\.zig\\'" . zig-mode)
        :custom ((lsp-zig-zls-executable . "/Users/tbm/zls/zls")
                 (zig-format-on-save . t)
                 (zig-format-show-buffer)))

    (leaf zoom-window
        :doc "Zoom in to a pane"
        :url "https://github.com/emacsorphanage/zoom-window"
        :commands (darkroom-mode)))



(provide 'managed)

;; Local Variables:
;; fill-column: 100
;; End:

;;; managed.el ends here
