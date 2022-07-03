;; -*- lexical-binding: t; -*-

;; ------------------------------ org-mode ------------------------------

(defun toy/init-org ()
    (interactive)
    (visual-line-mode)
    (setq fill-column 100)
    (turn-on-auto-fill)
    ;; do not count TODOs recursively
    (setq org-hierarchial-todo-statics nil))

(leaf org
    :mode ("\\.org\\'" . org-mode)
    :hook (org-mode-hook . toy/init-org)
    :config
    (setq org-directory "~/org")

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

    (leaf org-appear
        :doc "Uninline format on cursor"
        :url "https://github.com/awth13/org-appear"
        :config
        (setq org-appear-autolinks t)
        :hook (org-mode-hook . org-appear-mode))

    (leaf org-superstar
        :commands org-superstar-mode
        ;; :straight (org-superstar :type git :host github :repo "integral-dw/org-superstar-mode")
        ;; :straight '(org-superstar :fork (:host github :repo "thibautbenjamin/org-superstar-mode"))
        :hook (org-mode-hook . org-superstar-mode)
        :config
        (setq org-superstar-special-todo-items t))
    (setq org-hide-emphasis-markers t)

    ;; (leaf org-modern
    ;;     :ensure nil
    ;;     :hook (org-mode-hook . org-modern-mode)
    ;;     :straight (org-modern :type git :host github :repo "minad/org-modern"))

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
    (org-clock-persistence-insinuate)

    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

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

    ;; (leaf org-roam
    ;;     :custom
    ;;     (org-roam-directory . (file-truename "~/org/roam"))

    ;;     ;; :bind (("C-c n l" . org-roam-buffer-toggle)
    ;;     ;;        ("C-c n f" . org-roam-node-find)
    ;;     ;;        ("C-c n g" . org-roam-graph)
    ;;     ;;        ("C-c n i" . org-roam-node-insert)
    ;;     ;;        ("C-c n c" . org-roam-capture)
    ;;     ;;        ;; Dailies
    ;;     ;;        ("C-c n j" . org-roam-dailies-capture-today))

    ;;     :config
    ;;     ;; If you're using a vertical completion framework, you might want a more informative completion interface
    ;;     ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    ;;     (org-roam-db-autosync-mode)
    ;;     ;; (require 'org-roam-protocol)
    ;;     )

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

