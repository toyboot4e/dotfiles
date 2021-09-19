;; Language supports

(use-package rustic
    ;; FIXME: start automatically
    :mode ("\\.rs\\'" . rustic-mode)
    ;; `rustic` uses `rust-analyzer` by default`
    :hook (rustic-mode . lsp-deferred)
    :hook (rustic-mode . toy/init-rustic)
    :config
    ;; `rustic`, please don't format
    (setq rustic-format-trigger nil
          rustic-format-on-save nil
          rustic-lsp-format t
          ;; Don't show inlay hints for type parameters
          ;; (sideline of diagnostics are still enabled)
          lsp-rust-analyzer-server-display-inlay-hints nil
          )

    (defun toy/init-rustic ()
        (interactive)
        (visual-line-mode)
        (setq fill-column 100)
        (turn-on-auto-fill))

    ;; `lsp-mode`, please format on save
    (add-hook 'before-save-hook
              (_fn (when (eq 'rustic-mode major-mode) (lsp-format-buffer))))
    )

;; (use-package cargo
;;     :hook (rust-mode . cargo-minor-mode))

;; (progn ;; TODO: C#
;;     (use-package csharp-mode
;;         :defer t)
;;     (use-package omnisharp
;;         ;; https://github.com/OmniSharp/omnisharp-emacs#:~:text=omnisharp-emacs%20is%20a%20port,that%20works%20in%20the%20background.
;;         :defer t)
;;     )

(progn ;; zig
    (use-package zig-mode
        ;; https://github.com/ziglang/zig-mode
        :mode ("\\.zig\\'" . zig-mode)
        ;; :hook (zig-mode . lsp-deferred)
        :hook (zig-mode . flycheck-mode)
        :config
        (remove-hook 'before-save-hook 'zig-before-save-hook t)
        (setq zig-format-on-save t)
        ;; Don't show `*zig-fmt*` buffer, perferring `lsp-ui`
        (setq zig-format-show-buffer t))

    ;; (flycheck-define-checker zig
    ;;     "A zig syntax checker using the zig-fmt interpreter."
    ;;     :command ("zig" "fmt" (eval (buffer-file-name)))
    ;;     :error-patterns
    ;;     ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
    ;;     :modes zig-mode)
    ;; (add-to-list 'flycheck-checkers 'zig)

    ;; (setq lsp-zig-zls-executable "~/zls/zls")

    ;; ;; ZLS: https://github.com/zigtools/zls
    ;; (with-eval-after-load 'lsp-mode
    ;;     (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
    ;;     (lsp-register-client (make-lsp-client
    ;;                           :new-connection (lsp-stdio-connection lsp-zig-zls-executable)
    ;;                           :major-modes '(zig-mode)
    ;;                           :server-id 'zls))
    ;;     )
    )

(use-package ccls ;; C, C++
    :hook (c-mode . lsp-deferred)
    :hook (c++-mode . lsp-deferred)
    :config
    ;; FIXME: this is mac-only
    (setq ccls-executable "/usr/local/bin/ccls"))

(use-package go-mode
    :config
    (add-hook 'go-mode-hook
              ;; FIXME: it would work even if it's not in `go-mode`
              (_fn (add-hook 'before-save-hook #'lsp-format-buffer t t)
                   (add-hook 'before-save-hook #'lsp-organize-imports t t)
                   (lsp-mode)
                   (lsp-ui-mode)
                   (flycheck-mode)))
    )

(use-package idris-mode
    :hook (idirs-mode . lsp-deferred)
    )

;; (use-package html-ls)

(use-package tide
    :after (typescript-mode company flycheck)
    :hook ((typescript-mode . tide-setup)
           (typescript-mode . tide-hl-identifier-mode)
           (before-save . tide-format-before-save)))

;; see `locals.el'
;; (use-package tree-sitter
;;     :config
;;     ;; Replace the regex-based highlighting provided by font-lock-mode with
;;     ;; tree-based syntax highlighting
;;     ;; - limitedly:
;;     ;; (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
;;     ;; - globally:
;;     (global-tree-sitter-mode)
;;     (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;;     ;; tree-sitter-hl-mode
;;     )
;; 
;; (use-package tree-sitter-langs
;;     :after tree-sitter)
