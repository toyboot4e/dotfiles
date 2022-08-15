;; -*- lexical-binding: t -*-

(progn ;; sidebar settings
    (setq-default toy/sidebar-width 25)
    (setq-default toy/bottom-bar-height 7)
    (defvar toy/sidebar-imenu-buffer-name "@imenu")
    (defvar toy/bottom-vterm-buffer-name "⊥ vterm"))

;; --------------------------------------------------------------------------------
;; Shell
;; --------------------------------------------------------------------------------


;; TODO: shell
(leaf vterm
    :commands vterm--internal
    ;;
    )

;; (leaf vterm-toggle
;;                                         ;j    :custom
;;     (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
;;     (vterm-toggle-scope 'projectile)
;;     ;; :bind (("C-c t" . #'vterm-toggle)
;;     ;;        :map vterm-mode-map
;;     ;;        ("s-t" . #'vterm) ; Open up new tabs quickly
;;     ;;        )
;;     )

;; --------------------------------------------------------------------------------
;; Language supports
;; --------------------------------------------------------------------------------

;; (defun toy/lint-ja ()
;;     (interactive)
;;     ;; TODO: run only when the command is in scope
;;     (flycheck-disable-checker textlint-ja)
;;     (flycheck-define-checker textlint-en
;;         "A linter for text."
;;         :command ("textlint-editor.bash" "textlint-ja.json" source)
;;         :error-patterns
;;         ((warning line-start (file-name) ":" line ":" column ": "
;;                   (id (one-or-more (not (any " "))))
;;                   (message (one-or-more not-newline)
;;                            (zero-or-more "\n" (any " ") (one-or-more not-newline)))
;;                   line-end))
;;         :modes (text-mode markdown-mode adoc-mode gfm-mode org-mode))
;; 
;;     ;; (leaf flycheck-inline
;;     ;; FIXME: byte compile error?
;;     ;;     :hook (flycheck-mode-hook . flycheck-inline-mode))
;;     )
;; 
;; (defun toy/lint-en ()
;;     (interactive)
;;     ;; TODO: run only when the command is in scope
;;     (flycheck-disable-checker textlint-en)
;;     (flycheck-define-checker textlint-ja
;;         "A linter for text."
;;         :command ("textlint-editor.bash" "textlint-en.json" source)
;;         :error-patterns
;;         ((warning line-start (file-name) ":" line ":" column ": "
;;                   (id (one-or-more (not (any " "))))
;;                   (message (one-or-more not-newline)
;;                            (zero-or-more "\n" (any " ") (one-or-more not-newline)))
;;                   line-end))
;;         :modes (text-mode markdown-mode adoc-mode gfm-mode org-mode))
;; 
;;     ;; (leaf flycheck-inline
;;     ;; FIXME: byte compile error?
;;     ;;     :hook (flycheck-mode-hook . flycheck-inline-mode))
;;     )

(leaf prolog-mode
    :ensure nil
    :tag "builtin"

    ;; It's Prolog, not Perl!
    :mode "\\.l?pl\\'"
    :hook toy/on-prolog
    ;; :hook (prolog-mode-hook . toy/on-prolog)
    )

(defun toy/on-prolog ()
    (lsp-mode)
    (lsp-ui-mode)

    (add-to-list 'lsp-language-id-configuration '(prolog-mode . "prolog")

                 (lsp-register-client
                  (make-lsp-client
                   :new-connection
                   (lsp-stdio-connection (list "swipl"
                                               "-g" "use_module(library(lsp_server))."
                                               "-g" "lsp_server:main"
                                               "-t" "halt"
                                               "--" "stdio"))

                   :major-modes '(prolog-mode)
                   :priority 1
                   :multi-root t
                   :server-id 'prolog-ls))))

;; (leaf cargo
;;     :hook (rust-mode-hook . cargo-minor-mode))

(progn ;; TODO: C#
    (leaf csharp-mode)
    (leaf omnisharp
        ;; https://github.com/OmniSharp/omnisharp-emacs#:~:text=omnisharp-emacs%20is%20a%20port,that%20works%20in%20the%20background.
        )
    )

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

;; (leaf ccls ;; C, C++
;;     :hook (c-mode-hook . lsp-deferred)
;;     :hook (c++-mode-hook . lsp-deferred)
;;     :config
;;     ;; FIXME: this is mac-only
;;     (setq ccls-executable "/usr/local/bin/ccls"))

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
    :mode "\\.l?idr\\'"
    :hook lsp-deferred
    :custom
    (idris-interpreter-path . "~/.idris2/bin/idris2")
    :config
    (add-to-list 'lsp-language-id-configuration '(idris-mode . "idris2"))

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

(leaf typescript-mode
    :mode "\\.ts\\'"
    :init
    (define-derived-mode typescript-tsx-mode typescript-mode "TSX"
        "Major mode for editing TSX files.")
    (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
    (setq-default typescript-indent-level 2)

    :hook lsp-deferred
    ;; FIXME: `:hook` fails on byte compile
    :config
    (add-hook 'before-save-hook #'lsp-format-buffer))

(leaf vue-mode
    :config
    (add-hook 'vue-mode-hook #'lsp-deferred))

;; (leaf tide
;;     :after (typescript-mode company flycheck)
;;     :hook ((typescript-mode-hook . tide-setup)
;;            (typescript-mode-hook . tide-hl-identifier-mode)
;;            (before-save-hook . tide-format-before-save)))

;; NOTE: semantic-tokens in LS is enough?

;; (leaf tree-sitter
;;     :doc "Incremental parsing system"
;;     :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
;;     :added "2022-03-05"
;;     :emacs>= 25.1
;;     :config
;;     (global-tree-sitter-mode)
;;     (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (leaf tree-sitter-langs
;;     :after tree-sitter
;;     :config
;;     (tree-sitter-require 'tsx)
;;     (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

;; --------------------------------------------------------------------------------
;; Markup languages
;; --------------------------------------------------------------------------------

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

;; --------------------------------------------------------------------------------
;; @Sidebar (`lsp-ui-imenu-mode' and `neotree')
;; --------------------------------------------------------------------------------

(defun toy/imenu-get-nearest ()
    (interactive)
    "Returns `nil' or `(name . marker)' pair of the nearest item on `imenu'"

    ;; Thanks: https://emacs.stackexchange.com/questions/30673/next-prev-imenu-item-function
    ;; (imenu--make-index-alist)

    (let ((alist imenu--index-alist)
          (minoffset (point-max))
          base-point offset pair mark imstack result)
        (save-excursion
            (move-end-of-line 1)
            (setq base-point (point)))

        ;; Element = ("name" . marker)
        ;;         | ("submenu" ("name" . marker) ... )
        (while (or alist imstack)
            (if alist
                    (progn
                        (setq pair (car-safe alist)
                              alist (cdr-safe alist))
                        (cond
                         ((atom pair)) ;; Skip anything not a cons.

                         ((imenu--subalist-p pair)
                          (setq imstack   (cons alist imstack)
                                alist     (cdr pair)))

                         ((number-or-marker-p (setq mark (cdr pair)))
                          ;; REMARK: Allow zero, search direction = -1 (up)
                          (when (>= (setq offset (* (- mark base-point) -1)) 0)
                              (when (< offset minoffset) ;; Find the closest item.
                                  (setq minoffset offset
                                        result pair))))))

                ;; pop
                (setq alist   (car imstack)
                      imstack (cdr imstack))))

        result))

(defun toy/lsp-imenu-update-focus ()
    (interactive)
    "Move the `*lsp-ui-imenu*' buffer's point to the current item."
    (when (and (bound-and-true-p lsp-ui-mode) lsp-enable-imenu)
        (let ((window (get-buffer-window toy/sidebar-imenu-buffer-name)))
            (when window

                ;; get the name of the current item
                (let ((pair (toy/imenu-get-nearest)))
                    (when pair
                        (let ((pattern (concat "┃ " (car pair) "$")))

                            ;; search in the imenu buffer
                            (with-selected-window window
                                (goto-char 0)
                                (re-search-forward pattern nil 'no-error)

                                (move-beginning-of-line 1)
                                (scroll-right 1000)

                                ;; -----------------
                                (recenter)

                                (hl-line-mode 1)
                                (hl-line-highlight)))))))))

(defun toy/lsp-imenu-on-swtich-buffer ()
    (when (get-buffer toy/sidebar-imenu-buffer-name)
        (with-selected-window (get-buffer-window)
            (lsp-ui-imenu)
            (toy/lsp-imenu-update-focus))))

(add-hook 'post-command-hook #'toy/lsp-imenu-update-focus)
(add-hook 'window-selection-change-functions #'toy/lsp-imenu-update-focus)

;; --------------------------------------------------------------------------------
;; ⊥ Bottom bar  (`vterm')
;; --------------------------------------------------------------------------------

;; (get-buffer-create "⊥ scratch")

(defun toy/bottom-vterm ()
    (interactive)
    (let ((last-name nil))
        (when (boundp 'vterm)
            (setq last-name vterm-buffer-name))
        (setq vterm-buffer-name toy/bottom-vterm-buffer-name)
        (let ((buf (vterm--internal (lambda (_buf)))))
            ;; restore `vterm-buffer-name7
            (when last-name (setq vter-buffer-name last-name))

            (display-buffer-in-side-window buf '((side . bottom)))
            (let ((win (get-buffer-window buf)))
                (select-window win)
                (let ((dh (- toy/bottom-bar-height (window-height))))
                    (enlarge-window dh))))))

