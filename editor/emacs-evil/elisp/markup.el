;; Tiny language supports

;; ------------------------------ Snippets ------------------------------

;; ;; TODO: yasnippet
(use-package yasnippet
    :defer 3 ;; takes a while to load, so do it async
    :diminish yas-minor-mode
    :config (yas-global-mode)
    :custom (yas-prompt-functions '(yas-completing-prompt)))

(use-package yasnippet-snippets)

;; (evil-define-key 'normal 'global
;;     "\C-y" #'yas-expand
;;     )

;; ------------------------------ Translation ------------------------------

(use-package google-translate
    :custom
    (google-translate-default-source-language "ja")
    (google-translate-default-target-language "en")
    :config
    ;; (setq google-translate-translation-directions-alist
    ;;       '(("jp" . "en") ("en" . "jp")))

    ;; change backend (or else I got search failed error)
    (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
    (setq google-translate-backend-method 'curl)

    (require 'google-translate-smooth-ui))

;; ------------------------------ Configuration files ------------------------------

(use-package gitignore-mode
    :mode ("\\.gitignore" . gitignore-mode))

(use-package vimrc-mode
    :mode ("\\.vim" . vimrc-mode)
    :mode ("\\.nvim" . vimrc-mode))

(use-package fish-mode :defer t)

(use-package cmake-mode :defer t)

(use-package glsl-mode
    :mode ("\\.fs" . glsl-mode)
    :mode ("\\.vs" . glsl-mode)
    :mode ("\\.glsl" . glsl-mode)
    :mode ("\\.frag" . glsl-mode)
    :mode ("\\.vert" . glsl-mode))

;; ------------------------------ Markup languages ------------------------------

(use-package markdown-mode
    ;; https://jblevins.org/projects/markdown-mode/
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown")
    :config
    (evil-define-key 'normal markdown-mode-map
        "z1" (_fn (outline-hide-sublevels 1))
        "z2" (_fn (outline-hide-sublevels 2))
        "z3" (_fn (outline-hide-sublevels 3))
        "z4" (_fn (outline-hide-sublevels 4))
        "z5" (_fn (outline-hide-sublevels 5))
        "z6" (_fn (outline-hide-sublevels 6))
        "z9" (_fn (outline-hide-sublevels 9))
        "z0" #'evil-open-folds))

    (use-package adoc-mode
        ;; https://github.com/sensorflo/adoc-mode
        ;; c.f. `evil-lion` to align table
        :mode (("\\.adoc\\'" . adoc-mode))
        :hook (adoc-mode . toy/init-adoc-mode)
        :config
        (defun toy/init-adoc-mode ()
            (interactive)
            (outline-minor-mode)
            (setq-local electric-indent-mode nil))
        ;; disable auto indentation (locally)
        (add-hook 'LaTeX-mode-hook (lambda () (electric-indent-local-mode -1))))

(evil-define-key 'normal outline-minor-mode-map
    "z1" (_fn (outline-hide-sublevels 3))
    "z2" (_fn (outline-hide-sublevels 4))
    "z3" (_fn (outline-hide-sublevels 5))
    "z4" (_fn (outline-hide-sublevels 6))
    "z5" (_fn (outline-hide-sublevels 7))
    "z6" (_fn (outline-hide-sublevels 8))
    "z9" (_fn (outline-hide-sublevels 11))
    "z0" #'evil-open-folds
    )

(use-package yaml-mode :defer t)
(use-package ron-mode
    :mode (("\\.ron\\'" . ron-mode))
    ;; for `evil-nerd-commenter`:
    :hook (ron-mode . (lambda () (setq comment-start "// " comment-end "")))
    )

(use-package dhall-mode
    ;; setup: https://docs.dhall-lang.org/howtos/Text-Editor-Configuration.html
    ;; prelude: https://github.com/dhall-lang/dhall-lang/tree/v20.0.0/Prelude
    :mode "\\.dhall\\'"
    :hook ((dhall-mode . lsp-deferred))
    :config
    (setq
     ;; uncomment the next line to disable automatic format
     ;; dhall-format-at-save nil

     ;; comment the next line to use unicode syntax
     dhall-format-arguments (\` ("--ascii"))

     ;; header-line is obsoleted by lsp-mode
     dhall-use-header-line nil))

(use-package gnuplot-mode
    :mode (("\\.gp\\'" . gnuplot-mode))
    :defer t
    )

;; dhall-mode highlight the syntax and run dhall format on save
(use-package dhall-mode
    ;; https://docs.dhall-lang.org/
    ;; TODO: automatically enable dha;;-mode
    :mode "\\.dhall\\'" 
    ;; TODO: lsp-mode
    :hook (dhall-mode . lsp-deferred)
    :hook (dhall-mode . lsp-ui-mode)
    :config
    (setq  dhall-use-header-line nil
           dhall-format-arguments (\` ("--ascii"))
           ;; dhall-format-at-save nil
           ;; header-line is obsoleted by lsp-mode
           ))


;; ------------------------------ org-mode ------------------------------

(defun toy/init-org ()
    (interactive)
    (visual-line-mode)
    (setq fill-column 100)
    (turn-on-auto-fill)
    ;; do not count TODOs recursively
    (setq org-hierarchial-todo-statics nil)
    )

(use-package org
    :mode ("\\.org\\'" . org-mode)
    :hook (org-mode . toy/init-org)
    :config
    (setq org-directory "~/org")

    ;; MobileOrg
    (setq
     ;; Set to the name of the file where new notes will be stored
     org-mobile-inbox-for-pull "~/org/flagged.org"
     ;; Set to <your Dropbox root directory>/MobileOrg.
     org-mobile-directory "~/Dropbox/Apps/MobileOrg")

    ;; GTD: getting things done in a better way
    ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    (setq org-agenda-files
          (mapcar (lambda (path) (concat org-directory path))
                  '("/weekly.org"
                    "/agenda.org"
                    "/dev.org"
                    "/household.org"
                    "/read.org"
                    "/diary.org"
                    )))

    (setq org-log-done 'time
          org-src-fontify-natively t
          org-use-speed-commands t)

    (setq org-capture-templates
          '(("t" "Todo [inbox]" entry
             (file+headline "~/org/gtd/inbox.org" "Tasks")
             "* TODO %i%?")
            ("T" "Tickler" entry
             (file+headline "~/org/gtd/tickler.org" "Tickler")
             "* %i%? \n %^t")))

    (setq org-refile-targets
          '(("~/org/gtd/gtd.org" :maxlevel . 3)
            ("~/org/gtd/someday.org" :level . 1)
            ("~/org/gtd/tickler.org" :maxlevel . 2)))

    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

    (setq org-agenda-custom-commands
          '(("@" "Contexts"
             ((tags-todo "@email"
                         ((org-agenda-overriding-header "Emails")))
              (tags-todo "@phone"
                         ((org-agenda-overriding-header "Phone")))))))

    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)

    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

(evil-define-key 'normal org-mode-map
    "z1" (_fn (outline-hide-sublevels 1))
    "z2" (_fn (outline-hide-sublevels 2))
    "z3" (_fn (outline-hide-sublevels 3))
    "z4" (_fn (outline-hide-sublevels 4))
    "z5" (_fn (outline-hide-sublevels 5))
    "z6" (_fn (outline-hide-sublevels 6))
    "z7" (_fn (outline-hide-sublevels 7))
    "z8" (_fn (outline-hide-sublevels 8))
    "z9" (_fn (outline-hide-sublevels 9))
    "z0" #'evil-open-folds
    )

(use-package org-preview-html
    :after org-mode
    :defer t
    :commands org-preview-html-mode org-preview-html/preview)

(use-package evil-org
    :hook (org-mode  . evil-org-mode)
    :hook (evil-org-mode  . toy/init-evil-org)
    :config
    (defun toy/init-evil-org ()
        (interactive)
        (evil-org-set-key-theme)

        ;; TODO: [Terminal] Enable `<TAB>` losing `C-i` functionality (unfortunate hack)
        ;; (setq evil-want-C-i-jump (display-graphic-p))

        ;; do not overwrite `d`
        (evil-define-key 'motion 'evil-org-mode
            "d" 'evil-delete
            )
        )

    (add-hook 'org-mode-hook #'evil-org-mode)
    ;; (evil-org-agenda-set-keys)
    )

;; (use-package org-inlinetask
;;     :after org
;;     :config
;;     (org-inlinetask-insert-task))

(use-package org-bullets
    :commands org-bullets-mode
    :hook (org-mode . org-bullets-mode))
