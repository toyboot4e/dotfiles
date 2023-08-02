;; -*- lexical-binding: t; -*-

;; ------------------------------ org-mode ------------------------------

;; Do not count TODOs recursively
(setq-default org-hierarchial-todo-statics nil)

;; Do not indent source code
(setq-default org-edit-src-content-indentation 0)

;; Try `#+ATTR.*' keyword then fall back on the original width
(setq-default org-image-actual-width nil)

(defun toy/init-org ()
    (interactive)
    ;; Let's use logical lines. Line wrapping does not work well with Japanese text,
    ;; inserting needless whitespaces in output:
    (visual-line-mode)
    (setq fill-column 100)
    ;; (turn-on-auto-fill)
    )

(leaf org
    :mode ("\\.org\\'" . org-mode)
    :mode ("\\.org.draft\\'" . org-mode)
    :hook (org-mode-hook . toy/init-org)
    :hook (org-babel-after-execute . org-redisplay-inline-images)
    :config
    (setq org-directory "~/org")

    (evil-define-key 'normal org-mode-map
        "za" #'org-cycle)

    (progn ;; Setup diagram programs
        ;; Installation via `home-manager' is assumed:
        (setq org-ditaa-jar-path "~/.nix-profile/lib/ditaa.jar")
        (setq org-plantuml-jar-path "~/.nix-profile/lib/plantuml.jar")

        (org-babel-do-load-languages 'org-babel-load-languages
                                     (append org-babel-load-languages
                                             '((ditaa . t))
                                             '((dot . t))
                                             ))

        ;; (setq org-plantuml "plantuml")

        ;; FIXME: not wokring why
        ;; (org-babel-do-load-languages ‘org-babel-load-languages ‘((ditaa . t) (dot . t) (plantuml . t)))
        )

    (leaf simple-httpd
        :doc "`httpd-serve-directory' mainly for the org site"
        :config
        (defun toy/org-serve ()
            (interactive)
            (httpd-serve-directory "out")))

    (setq org-agenda-files
          (mapcar (lambda (path) (concat org-directory path))
                  '(
                    "/web.org"
                    "/ved.org"
                    "/dev-ink.org"
                    "/agenda.org"
                    "/bevy.org"
                    "/read.org"
                    )))

    (setq org-todo-keywords '((sequence "TODO(t)" "DONE(d)" "WIP(w)" "NOPE(n)")))

    ;; appearance
    (setq org-ellipsis "⤵")

    ;; configuration
    (setq org-log-done 'time
          org-src-fontify-natively t
          org-use-speed-commands t)

    ;; agenda and timer
    (setq org-agenda-custom-commands
          '(("@" "Contexts"
             ((tags-todo "@email"
                         ((org-agenda-overriding-header "Emails")))
              (tags-todo "@phone"
                         ((org-agenda-overriding-header "Phone")))))))

    (setq org-clock-persist t)

    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    :defer-config
    (org-clock-persistence-insinuate)

    (leaf org-appear
        :doc "Uninline format on cursor"
        :url "https://github.com/awth13/org-appear"
        :hook (org-mode-hook . org-appear-mode)
        :config
        (setq org-appear-autolinks t))

    ;; (leaf org-bullets
    ;;     :after org-mode
    ;;     :commands org-bullets-mode
    ;;     :hook (org-mode-hook\. org-bullets-mode))

    (leaf org-superstar
        :commands org-superstar-mode
        ;; :straight (org-superstar :type git :host github :repo "integral-dw/org-superstar-mode")
        ;; :straight '(org-superstar :fork (:host github :repo "thibautbenjamin/org-superstar-mode"))
        :hook (org-mode-hook . org-superstar-mode)
        :config
        (setq org-superstar-special-todo-items t))

    (setq org-hide-emphasis-markers t)

    ;; Very dependent on font style:
    ;; (leaf org-modern
    ;;     :hook (org-mode-hook . org-modern-mode))

    (progn ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
        ;; TODO: working?
        (font-lock-add-keywords 'org-mode
                                '(("^ *\\([-]\\) "
                                   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

        (custom-theme-set-faces
         'user
         '(org-block ((t (:inherit fixed-pitch))))
         '(org-code ((t (:inherit (shadow fixed-pitch)))))
         '(org-document-info ((t (:foreground "dark orange"))))
         '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
         '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
         '(org-link ((t (:underline t))))
         '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
         '(org-property-value ((t (:inherit fixed-pitch))) t)
         '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
         '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
         '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
         '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

    (setq prettify-symbols-alist '(("TODO" . "")
                                   ("WAIT" . "")
                                   ("WIP" . "")
                                   ("NOPE" . "")
                                   ("DONE" . "")

                                   ("[#A]" . "")
                                   ("[#B]" . "")
                                   ("[#C]" . "")
                                   ("[ ]" . "")
                                   ("[x]" . "")
                                   ("[X]" . "")
                                   ("[-]" . "")

                                   ("lambda" . "λ")
                                   ("|>" . "▷")
                                   ("<|" . "◁")
                                   ("->>" . "↠")
                                   ("->" . "→")
                                   ("<-" . "←")
                                   ("=>" . "⇒")
                                   ("<=" . "≤")
                                   (">=" . "≥")

                                   ("#+BEGIN_SRC" . "")
                                   ("#+END_SRC" . "―")
                                   (":PROPERTIES:" . "")
                                   (":END:" . "―")
                                   ("#+STARTUP:" . "")
                                   ("#+TITLE: " . "")
                                   ("#+RESULTS:" . "")
                                   ("#+NAME:" . "")
                                   ("#+ROAM_TAGS:" . "")
                                   ("#+FILETAGS:" . "")
                                   ("#+HTML_HEAD:" . "")
                                   ("#+SUBTITLE:" . "")
                                   ("#+AUTHOR:" . "")
                                   (":Effort:" . "")
                                   ("SCHEDULED:" . "")
                                   ("DEADLINE:" . "")

                                   ))
    (prettify-symbols-mode 1)

    ;; html view
    (leaf org-preview-html
        :commands org-preview-html-mode org-preview-html/preview)

    (leaf org-journal)

    ;; (leaf org-pdfview)

    ;; (setq org-capture-templates
    ;;       '(("t" "Todo [inbox]" entry
    ;;          (file+headline "~/org/gtd/inbox.org" "Tasks")
    ;;          "* TODO %i%?")
    ;;         ("T" "Tickler" entry
    ;;          (file+headline "~/org/gtd/tickler.org" "Tickler")
    ;;          "* %i%? \n %^t")))
    ;;
    ;; (setq org-refile-targets
    ;;       '(("~/org/gtd/gtd.org" :maxlevel . 3)
    ;;         ("~/org/gtd/someday.org" :level . 1)
    ;;         ("~/org/gtd/tickler.org" :maxlevel . 2)))

    ;; FIXME:
    ;; Warning (emacs): Org version mismatch.  Make sure that correct ‘load-path’ is set early in init.el

    ;; `org-roam' mappings:
    (evil-define-key 'normal org-mode-map
        " ol" #'org-roam-buffer-toggle
        " of" #'org-roam-node-find
        " og" #'org-roam-graph
        " oi" #'org-roam-node-insert
        " oc" #'org-roam-capture
        " oj" #'org-roam-dailies-capture-today
        " os" #'org-roam-db-sync
        " ota" #'org-roam-tag-add
        " otm" #'org-roam-tag-remove
        )

    (leaf org-roam
        :custom
        ;; any effect?
        ((org-roam-v2-ack . t)
         (org-roam-directory . "~/org-roam")

         (org-roam-capture-templates
          . '(
              ("d" "default" plain
               "%?"
               :if-new (file+head "%${slug}.org" "#+title: ${title}\n#+filetags: :problem:")
               ;; :if-new (file+head "%<%Y>-${slug}.org" "#+title: ${title}\n#+filetags: :problem:")
               ;; :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
               :unnarrowed t)

              ;; ("p" "plain" plain
              ;;  "%?"
              ;;  :if-new (file+head "%<%Y>-${slug}.org" "#+title: ${title}\n")
              ;;  ;; :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
              ;;  :unnarrowed t)

              ))
         )

        :config
        (org-roam-db-autosync-mode)

        ;; (org-roam-capture-templates
        ;;  '(("d" "default" plain
        ;;     "%?"
        ;;     :if-new (file+head "%<%Y>-${slug}.org" "#+title: ${title}\n")
        ;;     ;; :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
        ;;     :unnarrowed t)))

        ;;     :defer-config
        ;;     ;; If you're using a vertical completion framework, you might want a more informative completion interface
        ;;     ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
        ;;     (org-roam-db-autosync-mode)
        ;;     ;; (require 'org-roam-protocol)

        ;; `https://github.com/syl20bnr/spacemacs/issues/14137#issuecomment-735437329'
        (defadvice org-roam-insert (around append-if-in-evil-normal-mode activate compile)
            "If in evil normal mode and cursor is on a whitespace character, then go into
append mode first before inserting the link. This is to put the link after the
space rather than before."
            (let ((is-in-evil-normal-mode (and (bound-and-true-p evil-mode)
                                               (not (bound-and-true-p evil-insert-state-minor-mode))
                                               (looking-at "[[:blank:]]"))))
                (if (not is-in-evil-normal-mode)
                        ad-do-it
                    (evil-append 0)
                    ad-do-it
                    (evil-normal-state))))
        )

    (leaf org-roam-ui
        :after org-roam
        :custom
        ((org-roam-ui-sync-theme . t)
         (org-roam-ui-follow . t)
         (org-roam-ui-update-on-save . t)
         (org-roam-ui-open-on-start . t)))

    (leaf consult-org-roam
        :after org-roam

        :custom
        ;; Use `ripgrep' for searching with `consult-org-roam-search'
        (consult-org-roam-grep-func . #'consult-ripgrep)
        ;; Configure a custom narrow key for `consult-buffer'
        (consult-org-roam-buffer-narrow-key . ?r)
        ;; Display org-roam buffers right after non-org-roam buffers
        ;; in consult-buffer (and not down at the bottom)
        (consult-org-roam-buffer-after-buffers . t)

        :config
        (require 'consult-org-roam)
        ;; Activate the minor mode
        (consult-org-roam-mode 1)

        ;; Eventually suppress previewing for certain functions
        (consult-customize
         consult-org-roam-forward-links
         :preview-key (kbd "M-."))

        ;; :bind
        ;; ;; Define some convenient keybindings as an addition
        ;; ("C-c n e" . consult-org-roam-file-find)
        ;; ("C-c n b" . consult-org-roam-backlinks)
        ;; ("C-c n l" . consult-org-roam-forward-links)
        ;; ("C-c n r" . consult-org-roam-search)
        )

    )

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
    "z0" #'evil-open-folds)

