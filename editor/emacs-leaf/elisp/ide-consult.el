;; consult  -*- lexical-binding: t -*-

;; NOTE: `savehist-mode` is called in `evil.el`

(leaf consult
    ;; Required if we don't use default UI (like when using `vertico`)
    ;; :hook (completion-list-mode-hook . consult-preview-at-point-mode)

    :custom
    `((consult-preview-raw-size . 1024000)
      (consult-preview-key  . ,(kbd "C-M-p"))
      (consult-narrow-key   . "<"))

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

    (setq consult-preview-key (kbd "C-l"))
    (setq consult-narrow-key "<")
    ;; `which-key` alternative
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; detect project root with `projectile'
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-root-function #'projectile-project-root)

    ;; TODO: how can I use it like org-switchb
    (autoload 'org-buffer-list "org")
    (defvar org-buffer-source
        `(:name     "Org"
                    :narrow   ?o
                    :category buffer
                    :state    ,#'consult--buffer-state
                    :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))

    :defer-config
    (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)

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
    (setq vertico-count 20))

(leaf orderless
    :init
    (setq completion-styles '(orderless)
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
    :doc "alternative find-file/grep (space-separated multi word search with fd/rg)"
    :after consult
    :init
    (consult-customize affe-grep :preview-key (kbd "C-l"))
    ;; use `fd'
    (when (executable-find "fd")
        ;; (setq affe-find-command "fd -HI -t f")
        (setq affe-find-command "fd --color=never --full-path"))

    ;; show all the matching results
    (setq affe-count most-positive-fixnum)

    ;; use `orderless` with `affe`
    (setq affe-regexp-function #'orderless-pattern-compiler
          affe-highlight-function #'orderless--highlight))

;; FIXME: color of selected item

