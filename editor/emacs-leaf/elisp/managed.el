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
        :config
        (setq auto-package-update-delete-old-versions t
              auto-package-update-interval 7)
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
        :config
        (setq centaur-tabs--buffer-show-groups nil)
        (setq centaur-tabs-cycle-scope 'tabs)
        (setq centaur-tabs-set-bar 'under
              x-underline-at-descent-line t)
        (setq centaur-tabs-style "bar"
              centaur-tabs-height 24
              centaur-tabs-set-modified-marker t
              centaur-tabs-gray-out-icons 'buffer
              centaur-tabs-show-navigation-buttons nil
              centaur-tabs-set-icons (display-graphic-p))
        (centaur-tabs-mode t)
        :defer-config (centaur-tabs-headline-match) (centaur-tabs-group-by-projectile-project))

    (leaf clipetty
        :after evil
        :config
        (evil-ex-define-cmd "copy" #'clipetty-kill-ring-save))

    (leaf cmake-mode)

    (leaf company
        :hook (prog-mode-hook . company-mode)
        :init
        (setq company-idle-delay 0
              company-minimum-prefix-length 1
              company-selection-wrap-around t))

    (leaf company-box
        :if (display-graphic-p)
        :hook (company-mode-hook . company-box-mode))

    (leaf dhall-mode
        :mode "\\.dhall\\'"
        :hook (dhall-mode-hook . lsp-deferred)
        :hook (dhall-mode-hook . lsp-ui-mode)
        :config
        (setq dhall-use-header-line nil
              dhall-format-arguments `("--ascii")))

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
            :custom (minions-mode-line-lighter . "[+]")
            :config
            (setq doom-modeline-minor-modes t)
            (minions-mode)
            (minions-mode 1))

        :defer-config (doom-modeline-mode))

    (leaf editorconfig
        :config
        (editorconfig-mode 1))

    (leaf eldoc
        :ensure nil
        :tag "builtin")

    (leaf evil
        :init
        (setq evil-toggle-key "")
        (setq evil-want-keybinding nil)
        (setq evil-want-C-u-delete t
              evil-want-C-u-scroll t
              evil-want-Y-yank-to-eol t
              evil-move-cursor-back t
              evil-search-module 'evil-search)
        :config
        (evil-mode 1)
        (progn
            (evil-ex-define-cmd "ed"
                                (lambda nil
                                    (interactive)
                                    (evil-edit
                                     (concat user-emacs-directory "init.el"))))
            (evil-ex-define-cmd "s"
                                (lambda nil
                                    (interactive)
                                    (load-file
                                     (concat user-emacs-directory "init.el"))))
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
        :commands (evil-collection-dired-setup evil-collection-company-setup evil-collection-eww-setup evil-collection-elfeed-setup evil-collection-markdown-mode-setup evil-collection-consult-setup evil-collection-embark-setup evil-collection-magit-setup)
        :config
        (with-eval-after-load 'dired
            (evil-collection-dired-setup))

        (with-eval-after-load 'company
            (evil-collection-company-setup))

        (with-eval-after-load 'eww
            (evil-collection-eww-setup))

        (with-eval-after-load 'elfeed
            (evil-collection-elfeed-setup))

        (with-eval-after-load 'markdown-mode
            (evil-collection-markdown-mode-setup))

        (with-eval-after-load 'consult
            (evil-collection-consult-setup))

        (with-eval-after-load 'embark
            (evil-collection-embark-setup))

        (with-eval-after-load 'forge
            (evil-collection-forge-setup))

        (with-eval-after-load 'magit
            (evil-define-key 'normal magit-mode-map "zz" #'evil-scroll-line-to-center "z-" #'evil-scroll-line-to-bottom
                (kbd "z RET")
                #'evil-scroll-line-to-top
                (kbd "SPC RET")
                #'magit-diff-visit-worktree-file-other-window)
            (evil-define-key 'normal git-rebase-mode-map "C-j" git-rebase-move-line-down "C-u" git-rebase-move-line-up)
            (advice-add 'magit-section-forward :after
                        (lambda (&rest x)
                            (evil-scroll-line-to-top
                             (line-number-at-pos))))
            (advice-add 'magit-section-backward :after
                        (lambda (&rest x)
                            (evil-scroll-line-to-top
                             (line-number-at-pos))))
            (advice-add 'magit-section-forward-sibling :after
                        (lambda (&rest x)
                            (evil-scroll-line-to-top
                             (line-number-at-pos))))
            (advice-add 'magit-section-backward-sibling :after
                        (lambda (&rest x)
                            (evil-scroll-line-to-top
                             (line-number-at-pos))))
            (setq evil-collection-magit-use-z-for-folds t)
            (evil-collection-magit-setup)))

    (leaf evil-escape
        :doc "Smart escape with `jk` or `kj`"
        :init
        (setq evil-escape-key-sequence "jk"
              evil-escape-unordered-key-sequence t)
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
        :config
        (defun git-link-open-ln nil
            (interactive)
            (let ((git-link-open-in-browser t))
                (call-interactively #'git-link)))

        (defun git-link-today-ln nil
            (interactive)
            (let ((git-link-use-commit t))
                (call-interactively #'git-link)))

        (defun git-link-open-today-ln nil
            (interactive)
            (let ((git-link-use-commit t)
                  (git-link-open-in-browser t))
                (call-interactively #'git-link)))

        (defun git-link-open nil
            (interactive)
            (let ((git-link-use-single-line-number t))
                (git-link-open-ln)))

        (defun git-link-open-today nil
            (interactive)
            (let ((git-link-use-single-line-number t))
                (git-link-today-ln)))

        (defun git-link-open-today nil
            (interactive)
            (let ((git-link-use-single-line-number t))
                (git-link-open-today-ln))))

    (leaf glsl-mode
        :mode ("\\.fs" . glsl-mode)
        :mode ("\\.vs" . glsl-mode)
        :mode ("\\.glsl" . glsl-mode)
        :mode ("\\.frag" . glsl-mode)
        :mode ("\\.vert" . glsl-mode))

    (leaf gnuplot-mode
        :mode (("\\.gp\\'" . gnuplot-mode)))

    (leaf haskell-mode
        :url "https://github.com/haskell/haskell-mode"
        :hook (haskell-mode-hook . lsp-deferred)
        :hook (haskell-literate-mode-hook . lsp-deferred)
        :config
        (leaf lsp-haskell
            :after lsp-mode
            :custom
            :url "https://github.com/emacs-lsp/lsp-haskell"))

    (leaf helpful
        :bind ([remap describe-command]
               . helpful-command) ([remap describe-key]
               . helpful-key)
        :after evil
        :init
        (evil-define-key 'normal helpful-mode-map "q" #'kill-this-buffer)
        (evil-define-key 'normal 'global "K" #'helpful-at-point))

    (leaf highlight-indentation
        :doc "Minor modes for highlighting indentation"
        :url "https://github.com/antonj/Highlight-Indentation-for-Emacs"
        :config
        (highlight-indentation-mode))

    (leaf hl-todo
        :doc "highlight TODO, FIXME, etc."
        :config
        (setq hl-todo-highlight-punctuation ":"
              hl-todo-keyword-faces `(("TODO" warning bold)
                                      ("FIXME" error bold)
                                      ("HACK" font-lock-constant-face bold)
                                      ("REVIEW" font-lock-keyword-face bold)
                                      ("NOTE" success bold)
                                      ("WIP" font-lock-keyword-face bold)
                                      ("REMARK" success bold)
                                      ("DEPRECATED" font-lock-doc-face bold)))
        (global-hl-todo-mode 1))

    (leaf hydra)

    (leaf lsp-mode
        :after evil
        :commands (lsp-mode lsp-deferred)
        :hook (lsp-mode-hook . lsp-enable-which-key-integration)
        :hook (lsp-mode-hook . hs-minor-mode)
        :init
        (setq lsp-keymap-prefix nil)
        (setq lsp-idle-delay 0.5)
        (setq lsp-log-io nil
              lsp-trace nil
              lsp-print-performance nil)
        (setq lsp-eldoc-enable-hover nil
              lsp-signature-auto-activate nil
              lsp-signature-render-documentation nil
              lsp-completion-show-kind nil
              lsp-enable-symbol-highlighting nil
              lsp-headerline-breadcrumb-enable nil)
        (setq lsp-modeline-diagnostics-scope :workspace)
        (setq lsp-semantic-tokens-enable t)
        (setq lsp-session-file (concat user-emacs-directory "tmp/.lsp-session-v"))
        :config
        :defer-config (define-key evil-normal-state-map " l" lsp-command-map) (evil-define-key 'normal lsp-mode-map "K" #'lsp-describe-thing-at-point))

    (leaf lsp-ui
        :commands lsp-ui-mode
        :hook (lsp-mode-hook . lsp-ui-mode)
        :after evil
        :config
        (setq lsp-idle-delay 0.5
              lsp-ui-sideline-delay 0
              lsp-ui-doc-delay 0)
        (setq lsp-ui-doc-enable nil
              lsp-ui-doc-position 'top)
        (setq lsp-ui-sideline-show-diagnostics t
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-code-actions nil)
        (setq lsp-ui-imenu-window-width 30)
        (evil-define-key 'normal lsp-ui-imenu-mode-map
            (kbd "TAB")
            #'lsp-ui-imenu--view
            (kbd "RET")
            #'lsp-ui-imenu--visit))

    (leaf lua-mode)

    (leaf magit
        :url "https://github.com/magit/magit"
        :commands (magit)
        :after evil
        :config
        (setq transient-history-file (concat user-emacs-directory "tmp/transient/history.el")
              transient-values-file (concat user-emacs-directory "tmp/transient/values.el")
              transient-levels-file (concat user-emacs-directory "tmp/transient/levels.el"))
        (setq magit-log-section-commit-count 40)
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
        :init
        (setq markdown-command "multimarkdown")
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
                                'icons 'arrows)
              neo-window-position 'right
              neo-window-width 25
              neo-window-fixed-size nil
              neo-show-hidden-files t)
        :config
        (when (display-graphic-p)
            (add-hook 'neo-after-create-hook
                      (lambda (_)
                          (text-scale-adjust 0)
                          (text-scale-decrease 0.5))))
        (evil-define-key 'normal neotree-mode-map
            (kbd "RET")
            #'neotree-enter "oo" #'neotree-enter "ov" #'neotree-enter-vertical-split "oh" #'neotree-enter-horizontal-split "cd" #'neotree-change-root "cu" #'neotree-select-up-node "cc" #'neotree-copy-node "mc" #'neotree-create-node "md" #'neotree-delete-node "mr" #'neotree-rename-node "h" #'neotree-hidden-file-toggle "r" #'neotree-refresh "q" #'neotree-hide
            (kbd "TAB")
            'neotree-stretch-toggle))

    (leaf olivetti
        :doc "Zen mode *per buffer* (not per frame and that is great!)"
        :url "https://github.com/rnkn/olivetti"
        :commands (olivetti-mode)
        :custom (olivetti-body-width . 100))

    (leaf popup)

    (leaf projectile
        :leaf-defer nil
        :init
        (setq projectile-enable-caching t)
        :config
        (projectile-mode 1)
        (setq projectile-cache-file (concat user-emacs-directory "tmp/projectile.cache")
              projectile-known-projects-file (concat user-emacs-directory "tmp/projectile-bookmarks.eld")))

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

    (leaf rustic
        :custom (rustic-load-optional-libraries)
        :mode ("\\.rs\\'" . rustic-mode)
        :hook (rustic-mode-hook . lsp-deferred)
        :hook (rustic-mode-hook . toy/init-rustic)
        :config
        (setq rustic-format-trigger nil
              rustic-format-on-save nil
              rustic-lsp-format t
              lsp-rust-analyzer-server-display-inlay-hints nil)
        (add-hook 'before-save-hook
                  (_fn
                   (when (eq 'rustic-mode major-mode)
                       (lsp-format-buffer)))))

    (leaf tempel
        :doc "Tempo templates/snippets with in-buffer field editing"
        :url "https://github.com/minad/tempel")

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
        (add-to-list 'lsp-language-id-configuration
                     '(wgsl-mode . "wgsl"))
        (with-eval-after-load 'lsp-mode
            (lsp-register-client
             (make-lsp-client :new-connection
                              (lsp-stdio-connection "~/.cargo/bin/wgsl_analyzer")
                              :major-modes
                              '(wgsl-mode)
                              :server-id 'wgsl))))

    (leaf which-key
        :init
        (setq which-key-idle-delay 0.01
              which-key-idle-secondary-delay 0.01)
        :config
        (define-key help-map
            (kbd "M")
            'which-key-show-major-mode)
        (which-key-mode))

    (leaf yaml-mode)

    (leaf zig-mode
        :mode ("\\.zig\\'" . zig-mode)
        :config
        (setq lsp-zig-zls-executable "/Users/tbm/zls/zls")
        (setq zig-format-on-save t)
        (setq zig-format-show-buffer nil))

    (leaf zoom-window
        :doc "Zoom in to a pane"
        :url "https://github.com/emacsorphanage/zoom-window"
        :commands (darkroom-mode)))



(provide 'managed)

;; Local Variables:
;; fill-column: 100
;; End:

;;; managed.el ends here
