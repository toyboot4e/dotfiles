;; -*- lexical-binding: t -*-

;; TODO: Fix corfu

;; --------------------------------------------------------------------------------
;; `completing-read'
;; --------------------------------------------------------------------------------

(leaf consult
    ;; Required if we don't use default UI (like when using `vertico`)
    ;; :hook (completion-list-mode-hook . consult-preview-at-point-mode)

    :custom
    ((consult-preview-raw-size . 1024000)
     (consult-preview-key  . "C-l")
     (consult-narrow-key   . "<")
     (consult-async-min-input . 2)
     (register-preview-delay . 0)
     (register-preview-function #'consult-register-format)
     (xref-show-xrefs-function . #'consult-xref)
     (xref-show-definitions-function . #'consult-xref))

    :init
    ;; adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

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

(leaf consult-flycheck
    )

(leaf consult-lsp
    :after (consult lsp)
    :config
    (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(leaf vertico
    :doc "Show minibuffer items in rows"
    :hook (after-init-hook . vertico-mode)
    :custom
    ((vertico-cycle . t)
     (vertico-count . 20)
     (vertico-scroll-margin . 4)))

(leaf orderless
    :doc "Find with space-separated components in any order"
    :custom
    ((completion-styles . '(orderless partial-completion basic))
     (completion-category-defaults .  nil)
     ;; TODO: ?
     (completion-category-overrides . '((file (styles basic partial-completion))))
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
    :url "https://github.com/oantolin/embark"
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

;; Use `orderless':
;; <https://github.com/minad/consult/wiki/Home/a0e391f8416e98b8d8319d62fb40b64f939b9fd1#use-orderless-as-pattern-compiler-for-consult-grepripgrepfind>
;; better regex (copied from the README)
(defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
(setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

;; OPTION 2: Activate only for some commands, e.g., consult-ripgrep!
;; (defun consult--with-orderless (&rest args)
;;     (minibuffer-with-setup-hook
;;             (lambda ()
;;                 (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
;;         (apply args)))
;; (advice-add #'consult-ripgrep :around #'consult--with-orderless)

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
     (corfu-preselect . 'prompt)       ;; Do not select the first candidate
     (coruf-popupinfo-delay . 0.1)
     (tab-always-indent . 'complete)
     (completion-cycle-threshold .  3)
     (corfu-auto-prefix . 3))

    :config
    (global-corfu-mode)

    ;; https://github.com/minad/corfu#completing-in-the-minibuffer
    (defun corfu-enable-in-minibuffer ()
        "Enable Corfu in the minibuffer."
        (when (local-variable-p 'completion-at-point-functions)
            ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
            (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                        corfu-popupinfo-delay nil)
            (corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

    )

(leaf kind-icon
    :url "https://github.com/jdtsmith/kind-icon"
    ;; :custom (kind-icon-default-face . 'corfu-default)
    :config
    ;; FIXME: void?
    ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    (add-hook 'my-completion-ui-mode-hook
   	          (lambda ()
   	              (setq completion-in-region-function
   		                (kind-icon-enhance-completion
   		                 completion-in-region-function)))))

;; Deprecated:
;; (leaf corfu-doc
;;     :url "https://github.com/galeo/corfu-doc"
;;     :when (display-graphic-p)
;;     :hook (corfu-mode-hook . corfu-doc-mode))
;; :init
;; (global-corfu-mode)

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

;; (leaf popon
;; :url "https://codeberg.org/akib/emacs-popon"
;; :unless (display-graphic-p)
;; :ensure nil
;; :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon"))

(leaf corfu-popupinfo
    ;; Today it comes with `corfu'.
    :ensure nil
    :after corfu
    :custom ((coruf-popupinfo-delay . 0))
    :hook (corfu-mode-hook . corfu-popupinfo-mode))

(leaf corfu-terminal
    :url "https://codeberg.org/akib/emacs-corfu-terminal"
    :after (corfu corfu-popupinfo)
    :unless (display-graphic-p)
    :ensure nil
    :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal")
    :hook (corfu-mode-hook . corfu-terminal-mode))
;; :config
;; (corfu-terminal-mode +1))

;; TODO: deprecated?
;; (leaf corfu-doc-terminal
;;     :url "https://codeberg.org/akib/emacs-corfu-doc-terminal"
;;     :unless (display-graphic-p)
;;     :ensure nil
;;     :straight (corfu-doc-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal")
;;     :config
;;     (corfu-doc-terminal-mode +1))

;; --------------------------------------------------------------------------------
;; Snippets
;; --------------------------------------------------------------------------------

(leaf tempel
    :doc "Tempo templates/snippets with in-buffer field editing"
    :url "https://github.com/minad/tempel"
    :config
    (setq tempel-path (concat user-emacs-directory "tempel-snippets.el"))

    ;; FIXME: not working well?
    (defun tempel-setup-capf nil
        (setq-local completion-at-point-functions
                    (cons #'tempel-expand completion-at-point-functions)))
    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf)

    ;; TODO: Replace with `embark' (probably).
    ;; TODO: Use `corfu' and `cape' rather than direct call.
    (evil-define-key 'insert 'global
        (kbd "C-y") #'tempel-complete
        (kbd "C-l") #'tempel-insert
        (kbd "C-t") #'tempel-expand)

    ;; ;; FIXME: not working since it's not a mode?
    ;; (evil-define-key 'insert 'tempel-map
    ;;     "C-n" #'tempel-next
    ;;     "C-p" #'tempel-previous
    ;;     "C-<RET>" #'tempel-done)

    ;; ;; FIXME: not working since it's not a mode?
    ;; (evil-define-key 'normal 'tempel-map
    ;;     "C-<RET>" #'tempel-done)
    )

