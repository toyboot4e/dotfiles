;; -*- lexical-binding: t -*-

;; --------------------------------------------------------------------------------
;; Builitin UI
;; --------------------------------------------------------------------------------

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
(setq scroll-preserve-screen-position t
      scroll-conservatively 100
      scroll-margin 3)

;; --------------------------------------------------------------------------------
;; Builtin packages
;; --------------------------------------------------------------------------------

;; TODO: put them in leaf

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

;; --------------------------------------------------------------------------------
;; GUI/Terminal
;; --------------------------------------------------------------------------------

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
          '(("Noto Sans Mono CJK JP" . 1.25))))

;; If on terminal
(when (not (display-graphic-p))
    ;; Two exclusive options:
    ;; 1. use left click to move cursor:
    (xterm-mouse-mode 1)

    ;; 2. use left click to select (and copy):
    ;; (xterm-mouse-mode -1)

    ;; Use mouse wheel for scrolling
    ;; TODO: do not map it to a specific command.
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; --------------------------------------------------------------------------------
;; Elisp
;; --------------------------------------------------------------------------------

(setq-default lisp-body-indent 4
              indent-tabs-mode nil
              tab-width 4)

;; --------------------------------------------------------------------------------
;; Custom package settings
;; --------------------------------------------------------------------------------

(progn ;; sidebar settings
    (setq-default toy/sidebar-width 25)
    (setq-default toy/bottom-bar-height 7)
    (defvar toy/sidebar-imenu-buffer-name "@imenu")
    (defvar toy/bottom-vterm-buffer-name "⊥ vterm"))

(defun toy/smart-recenter ()
    "Recenter or scroll to just before EoF"
    ;; TODO: taken into account visual line
    (interactive)
    (let ((max-ln (line-number-at-pos (buffer-size)))
          (ln (line-number-at-pos (point)))
          (current-scroll (line-number-at-pos (window-start)))
          (h (window-body-height)))
        (let ((smart-max-scroll (max 0 (+ scroll-margin (- max-ln (- h 1)))))
              (scroll (max 0 (- ln (/ h 2)))))
            (scroll-down (- current-scroll (min smart-max-scroll scroll)))
            )))

;; --------------------------------------------------------------------------------
;; Builtin modes
;; --------------------------------------------------------------------------------

(defun toy/info-url ()
    "Returns current info URL"
    (interactive)
    (let* (;; `(emacs) Case Conversion'
           (name (Info-copy-current-node-name))
           (space-offset (string-match " " name))
           ;; `(emacs)'
           (manual-name (substring name 1 (- space-offset 1)))
           ;; `Case Conversion'
           (page-name (string-replace " " "-" (substring name (+ space-offset 1)))))
        ;; `https://www.gnu.org/software/emacs/manual/html_node/elisp/Case-Conversion.html'
        (message (concat "https://www.gnu.org/software/emacs/manual/html_node/" manual-name "/" page-name ".html"))))

(defun toy/info-open-browser ()
    "Opens the current info with the default browser"
    (interactive)
    (browse-url (toy/info-url)))

(defun toy/last-message()
    "Retrieves the last echoed message from the `Messages' buffer"
    (save-excursion
        (set-buffer "*Messages*")
        (save-excursion
            (forward-line (- 1 num))
            (backward-char)
            (let ((end (point)))
                (forward-line 0)
                (buffer-substring-no-properties (point) end)))))

;; --------------------------------------------------------------------------------
;; Textlint
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

;; --------------------------------------------------------------------------------
;; Language supports
;; --------------------------------------------------------------------------------

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

(leaf idris-mode
    :after lsp-mode
    :mode "\\.l?idr\\'"
    :hook lsp-deferred
    :custom
    (idris-interpreter-path . "idris2")
    :config
    (add-to-list 'lsp-language-id-configuration '(idris-mode . "idris2"))

    ;; (with-eval-after-load 'lsp-mode
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "idris2-lsp")
      :major-modes '(idris-mode)
      :server-id 'idris2-lsp)))

;; Common Lisp

(leaf folding-mode
    :ensure nil
    :straight (folding-mode :type git :host github :repo "jaalto/project-emacs--folding-mode"))

(leaf slime
    :if (file-exists-p "~/.roswell/helper.el")
    ;; :ensure slime-company
    :init (load "~/.roswell/helper.el")
    :custom (inferior-lisp-program . "sbcl")
    ;; :custom (inferior-lisp-program "ros -Q run")
    ;; :config (slime-setup '(slime-fancy slime-company))
    )

;; (leaf html-ls)

(leaf prettier
    :doc "Aggressive source format on save.
Maybe use `dir-locals.el' or similars rather than to hooks:
https://github.com/jscheid/prettier.el?tab=readme-ov-file#enabling-per-file--per-directory"
    :custom (prettier-inline-errors-flag . t)
    :hook (typescript-mode-hook . prettier-mode)
    :hook (css-mode-hook . prettier-mode))

(leaf typescript-mode
    :mode "\\.ts\\'"
    :init
    (define-derived-mode typescript-tsx-mode typescript-mode "TSX"
        "Major mode for editing TSX files.")
    (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
    (setq-default typescript-indent-level 2)

    :hook (typescript-mode-hook . lsp-deferred)
    ;; Using prettier instaead, but both should be unified
    ;; :config
    ;; (add-hook 'before-save-hook #'lsp-format-buffer)
    )

(leaf vue-mode
    :config
    (add-hook 'vue-mode-hook #'lsp-deferred))

(leaf emmet-mode
    :hook
    ((html-mode-hook . emmet-mode)
     (web-mode-hook . emmet-mode)
     (css-mode-hook . emmet-mode)
     (typescript-tsx-mode-hook . emmet-mode)
     (vue-mode-hook . emmet-mode)))

;; (leaf tide
;;     :after (typescript-mode company flycheck)
;;     :hook ((typescript-mode-hook . tide-setup)
;;            (typescript-mode-hook . tide-hl-identifier-mode)
;;            (before-save-hook . tide-format-before-save)))

;; NOTE: semantic-tokens in LS is enough?

(leaf tree-sitter
    :doc "Incremental parsing system"
    :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(leaf tree-sitter-langs
    :after tree-sitter
    :config
    ;; TODO: need this check?
    (with-eval-after-load 'typescript-tsx-mode
        (tree-sitter-require 'tsx)
        (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))
    ;; (with-eval-after-load 'rust-mode
    ;;     )
    )

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
        "z0" #'evil-open-folds))

;; --------------------------------------------------------------------------------
;; @Sidebar (`lsp-ui-imenu-mode' and `neotree')
;; --------------------------------------------------------------------------------

(defun toy/imenu-get-nearest ()
    "Returns `nil' or `(name . marker)' pair of the nearest item on `imenu'"
    (interactive)

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

;; FIXME: error
(defun toy/lsp-imenu-update-focus ()
    "Move the `*lsp-ui-imenu*' buffer's point to the current item."
    (interactive)
    (when (and (bound-and-true-p lsp-ui-mode) (bound-and-true-p lsp-enable-imenu))
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
                                ;; (toy/smart-recenter)

                                (hl-line-mode 1)
                                (hl-line-highlight)))))))))

;; (defun toy/lsp-imenu-on-swtich-buffer ()
;;     (when (get-buffer toy/sidebar-imenu-buffer-name)
;;         (with-selected-window (get-buffer-window)
;;             (lsp-ui-imenu)
;;             (toy/lsp-imenu-update-focus))))

;; (add-hook 'post-command-hook #'toy/lsp-imenu-update-focus)
;; (add-hook 'window-selection-change-functions #'toy/lsp-imenu-update-focus)
;; (add-hook 'window-configuration-change-hook #'toy/lsp-imenu-update-focus)

;; --------------------------------------------------------------------------------
;; ⊥ Bottom bar  (`vterm' terminal)
;; --------------------------------------------------------------------------------

;; (get-buffer-create "⊥ scratch")

;; TODO: relace with `eat'
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
                (let ((dh (- toy/bottom-bar-height (window-body-height))))
                    (enlarge-window dh))))))

;; koka
(leaf koka-mode
    :ensure nil
    :load-path `,(concat user-emacs-directory "straight/repos/koka/support/emacs/")
    :straight (koka-mode :type git :host github :repo "koka-lang/koka")
    :require t)

;; SML (NOTE: It conflicts with the OCaml settings)
;; (leaf sml-mode
;;     :url "http://elpa.gnu.org/packages/sml-mode.html"
;;     :mode ("\\.ml\\'" . sml-mode)
;;     :mode ("\\.mli\\'" . sml-mode))

(progn ;; OCaml
    (add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
    (autoload 'merlin-mode "merlin" "Merlin mode" t)
    (add-hook 'tuareg-mode-hook #'merlin-mode)
    (add-hook 'caml-mode-hook #'merlin-mode))

(leaf ocamlformat
    :init
    (defun reserve-ocaml-format-on-save ()
        (add-hook 'before-save-hook 'ocamlformat-before-save))
    :hook (tuareg-mode-hook . reserve-ocaml-format-on-save))

(leaf flycheck-ocaml
    :custom (merlin-error-after-save . nil)
    :hook ((tuareg-mode-hook . flycheck-ocaml-setup)
           (tuareg-mode-hook . flycheck-mode)))

(leaf flycheck-inline
    :hook (tuareg-mode-hook . flycheck-inline-mode))

(leaf dune)
(leaf utop
    :hook (tuareg-mode-hook . utop-minor-mode))

