;; ------------------------------ Ivy ------------------------------

(setq magit-completing-read-function 'ivy-completing-read)
(setq projectile-completion-system 'ivy)

;; ------------------------------ Ivy ------------------------------

;; use Ivy for `tab-bar-switch-to-tab`
(defun advice-completing-read-to-ivy (orig-func &rest args)
    (interactive
     (let* ((recent-tabs (mapcar (lambda (tab)
                                     (alist-get 'name tab))
                                 (tab-bar--tabs-recent))))
         (list (ivy-completing-read "Switch to tab by name (default recent): "
                                    recent-tabs nil nil nil nil recent-tabs))))
    (apply orig-func args))
(advice-add #'tab-bar-switch-to-tab :around #'advice-completing-read-to-ivy)

(use-package ivy
    ;; https://github.com/abo-abo/swiper
    :init
    (setq ivy-use-virtual-buffers nil ; show recentf and bookmarks?
          ivy-count-format "(%d/%d) "
          ivy-height 20
          ivy-truncate-lines t        ; trancate or wrap
          ivy-wrap t                  ; cycle, please!
          )

    :bind (:map ivy-minibuffer-map
                ;; Vim-like
                ("C-f" . ivy-scroll-up-command)
                ("C-b" . ivy-scroll-down-command)
                ;; press `C-l` to preview (default: `C-M-m`)
                ("C-l" . ivy-call)
                ;; press `C-k k` to kill buffer (without closing Ivy)
                ("C-k" . ivy-switch-buffer-kill)
                ;; press `C-,` to open menu
                ;; NOTE: never map it to `C-m`; it's carriage-return in terminal
                ("C-," . ivy-dispatching-call)
                ("C-i" . ivy-immediate-done)
                ;; history
                ("C-d" . ivy-next-history-element)
                ("C-u" . ivy-previous-history-element)
                )

    :config (ivy-mode))

(use-package ivy-rich
    ;; https://github.com/Yevgnen/ivy-rich
    :after ivy
    :init (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    :config (ivy-rich-mode 1))

(use-package counsel
    :after ivy
    :bind
    ;; override default functions (`C-h f` etc.)
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-bindings] . counsel-descbinds)
    ;; see also: `C-h M` (which is mapped to `which-key-show-major-mode`)
    )

(use-package counsel-projectile
    :after (evil counsel projectile)
    :config
    ;; overwrite key mappings for `i_CTRL-r` (with Ivy height 20)
    (evil-define-key 'insert 'global "\C-r" #'counsel-evil-registers)
    ;; NOTE: we can't use `counsel-evil-registers` in ex mode because both of them use minibuffer
    ;; (add-to-list 'ivy-height-alist '(counsel-evil-registers . 20))
    )

(use-package swiper
    :after evil
    :config (evil-define-key 'normal 'global
                "*" 'swiper-thing-at-point
                ))

(use-package ivy-hydra :defer t)

(use-package lsp-ivy
    :after lsp
    :commands (lsp-ivy-workspace-symbol
               lsp-ivy-global-workspace-symbol))
