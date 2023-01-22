;; -*- lexical-binding: t -*-

;; --------------------------------------------------------------------------------
;; `completing-read'
;; --------------------------------------------------------------------------------

(leaf consult
    ;; Required if we don't use default UI (like when using `vertico`)
    ;; :hook (completion-list-mode-hook . consult-preview-at-point-mode)

    :custom
    `((consult-preview-raw-size . 1024000)
      (consult-preview-key  . ,(kbd "C-l"))
      (consult-narrow-key   . "<")
      (consult-async-min-input . 2))

    ;; strictly evaluated
    :init
    ;; for faster register preview
    (setq register-preview-delay 0
          register-preview-function #'consult-register-format)

    ;; adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    :config
    ;; use `fd`
    (when (executable-find "fd")
        (setq consult-find-command "fd --color=never --full-path ARG OPTS"))

    ;; `which-key` alternative
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; detect project root with `projectile'
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-root-function #'projectile-project-root)

    :defer-config
    ;; TODO: how can I use it like org-switchb
    ;; FIXME: I fail to defer `org-mode' loading with the org-mode source buffer
    ;; (autoload 'org-buffer-list "org")
    ;; (defvar org-buffer-source
    ;;     `(:name     "Org"
    ;;                 :narrow   ?o
    ;;                 :category buffer
    ;;                 :state    ,#'consult--buffer-state
    ;;                 :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
    ;; (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)

    (add-to-list 'consult-buffer-sources
                 (list :name     "Tabs"
                       :narrow   ?t
                       :category 'tab
                       :face     'font-lock-doc-face
                       :open     #'tab-bar-select-tab-by-name
                       :items    #'(lambda () (mapcar #'(lambda (tab) (cdr (assq 'name tab))) (tab-bar-tabs))))
                 'append))

(leaf consult-ghq)

(leaf consult-dir)

(leaf consult-lsp
    :after (consult lsp)
    :config
    (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(leaf vertico
    :doc "Show minibuffer items in rows"
    :hook (after-init-hook . vertico-mode)
    :preface
    (setq vertico-cycle t)
    (setq vertico-count 20)
    (setq vertico-scroll-margin 4))

(leaf orderless
    :doc "Find with space-separated components in any order"
    :init
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion))))
          ;; completion-category-overrides '((file (styles . (initials))))
          ))

(leaf emacs
    :init
    ;; add prompt indicator to `completing-read-multiple'.
    (defun crm-indicator (args)
        (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; grow/shrink minibuffer
    ;;(setq resize-mini-windows t)

    ;; forbit the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    (setq enable-recursive-minibuffers t))

(leaf marginalia
    :doc "Richer annotations in minibuffer"
    :hook (after-init-hook . marginalia-mode)
    :bind
    ;; press `C-q` to add/remove metadata
    (:minibuffer-local-map ("\C-q" . marginalia-cycle))

    :init
    (marginalia-mode)

    ;; automatically save the configuration
    (advice-add #'marginalia-cycle :after
                (lambda ()
                    (let ((inhibit-message t))
                        (customize-save-variable 'marginalia-annotator-registry
                                                 marginalia-annotator-registry))))

    ;; annotate `projectile-switch-project'
    (add-to-list 'marginalia-prompt-categories '("Switch to project" . file))

    ;; annotate `org-switchb'
    (add-to-list 'marginalia-prompt-categories '("Org buffer" . buffer))

    ;; FIXME: annotate `tab-bar-*-tab-by-name'
    (add-to-list 'marginalia-prompt-categories '("tab by name" . tab)))

(leaf embark
    :doc "Context menu in minibufffers"
    :bind
    (:minibuffer-local-map
     (("C-x" . embark-act)
      ;; alternative for `describe-bindings'
      ("C-h B" . embark-bindings)))

    :init
    (setq prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

(leaf embark-consult
    :hook
    (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf affe
    :doc "Alternatives to find-file and grep"
    :after (consult orderless)
    :init
    (consult-customize affe-grep :preview-key (kbd "C-l"))
    ;; use `fd'
    (when (executable-find "fd")
        ;; (setq affe-find-command "fd -HI -t f")
        (setq affe-find-command "fd --color=never --full-path -t f"))

    ;; show all the matching results
    (setq affe-count most-positive-fixnum)

    ;; better regex (copied from the README)
    (defun affe-orderless-regexp-compiler (input _type _ignorecase)
        (setq input (orderless-pattern-compiler input))
        (cons input (lambda (str) (orderless--highlight input str))))
    (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
    )

;; --------------------------------------------------------------------------------
;; Completion
;; --------------------------------------------------------------------------------

(leaf corfu
    :doc "Be sure to configure `lsp-mode' with `corfu'"
    :url "https://github.com/minad/corfu"
    ;; Optional customizations
    :custom
    ((corfu-cycle .  t)
     (corfu-auto . t)                 ;; Enable auto completion
     (corfu-count . 30)
     ;; (corfu-separator . ?\s)          ;; Orderless field separator
     ;; (corfu-quit-at-boundary . nil)   ;; Never quit at completion boundary
     ;; (corfu-quit-no-match . nil)      ;; Never quit, even if there is no match
     ;; (corfu-preview-current . nil)    ;; Disable current candidate preview
     ;; (corfu-preselect-first . nil)    ;; Disable candidate preselection
     ;; (corfu-on-exact-match . nil)     ;; Configure handling of exact matches
     ;; (corfu-echo-documentation . nil) ;; Disable documentation in the echo area
     ;; (corfu-scroll-margin . 5)        ;; Use scroll margin
     (coruf-popupinfo-delay . 0)
     )
    :hook (corfu-mode-hook . corfu-popupinfo-mode)

    :pre-setq ((tab-always-indent . 'complete)
               (corfu-cycle . t)
               (corfu-auto . t)
               (corfu-auto-prefix . 1)
               (corfu-preselect-first . nil))

    :config
    (leaf kind-icon
        :url "https://github.com/jdtsmith/kind-icon"
        :custom (kind-icon-default-face . 'corfu-default)
        :config
        ;; FIXME: void?
        (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

    ;; Deprecated:
    ;; (leaf corfu-doc
    ;;     :url "https://github.com/galeo/corfu-doc"
    ;;     :when (display-graphic-p)
    ;;     :hook (corfu-mode-hook . corfu-doc-mode))
    ;; :init
    ;; (global-corfu-mode)

    (leaf corfu-popupinfo
        :custom ((coruf-popupinfo-delay . 0))
        :hook (corfu-mode-hook . corfu-popupinfo-mode)
        )
    )

;; A few more useful configurations...
(leaf emacs
    :init
    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)

    ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
    ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete))

(leaf cape
    :url "https://github.com/minad/cape"

    ;; Bind dedicated completion commands
    ;; Alternative prefix keys: C-c p, M-p, M-+, ...
    ;; :bind (("C-c p p" . completion-at-point) ;; capf
    ;;        ("C-c p t" . complete-tag)        ;; etags
    ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
    ;;        ("C-c p h" . cape-history)
    ;;        ("C-c p f" . cape-file)
    ;;        ("C-c p k" . cape-keyword)
    ;;        ("C-c p s" . cape-symbol)
    ;;        ("C-c p a" . cape-abbrev)
    ;;        ("C-c p i" . cape-ispell)
    ;;        ("C-c p l" . cape-line)
    ;;        ("C-c p w" . cape-dict)
    ;;        ("C-c p \\" . cape-tex)
    ;;        ("C-c p _" . cape-tex)
    ;;        ("C-c p ^" . cape-tex)
    ;;        ("C-c p &" . cape-sgml)
    ;;        ("C-c p r" . cape-rfc1345))

    :init
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    ;;(add-to-list 'completion-at-point-functions #'cape-history)
    ;; Programming language keyword
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-tex)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;; ELisp
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)
    )

(unless (display-graphic-p)
    (leaf popon
        :url "https://codeberg.org/akib/emacs-popon"
        :unless (display-graphic-p)
        :ensure nil
        :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon"))

    (leaf corfu-terminal
        :url "https://codeberg.org/akib/emacs-corfu-terminal"
        :after popon
        :unless (display-graphic-p)
        :ensure nil
        :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal")
        :config
        (corfu-terminal-mode +1))

    ;; TODO: deprecated?
    ;; (leaf corfu-doc-terminal
    ;;     :url "https://codeberg.org/akib/emacs-corfu-doc-terminal"
    ;;     :unless (display-graphic-p)
    ;;     :ensure nil
    ;;     :straight (corfu-doc-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal")
    ;;     :config
    ;;     (corfu-doc-terminal-mode +1))

    )

;; --------------------------------------------------------------------------------
;; Snippets
;; --------------------------------------------------------------------------------

(leaf tempel
    :doc "Tempo templates/snippets with in-buffer field editing"
    :url "https://github.com/minad/tempel"
    :config
    (setq tempel-path (concat user-emacs-directory "templates.el"))

    ;; FIXME: not working well?
    (defun tempel-setup-capf nil
        (setq-local completion-at-point-functions
                    (cons #'tempel-expand completion-at-point-functions)))
    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf)

    (evil-define-key 'insert 'global
        (kbd "C-j")
        #'tempel-complete
        (kbd "C-l")
        #'tempel-insert
        (kbd "C-t")
        #'tempel-expand))

