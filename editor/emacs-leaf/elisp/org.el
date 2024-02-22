;; -*- lexical-binding: t; -*-

;; ------------------------------ org-mode ------------------------------

(defun toy/init-org ()
    (interactive)
    ;; Let's use logical lines. Line wrapping does not work well with Japanese text,
    ;; inserting needless whitespaces in output:
    (visual-line-mode)
    (setq fill-column 100)
    ;; (turn-on-auto-fill)
    )

;; TODO: preview latex
(leaf org
    :mode ("\\.org\\'" . org-mode)
    :hook (org-mode-hook . toy/init-org)
    :hook (org-babel-after-execute . org-redisplay-inline-images)
    :custom
    ((org-directory . "~/org")
     (org-cycle-emulate-tab . nil)
     (org-ditaa-jar-path . "~/.nix-profile/lib/ditaa.jar")
     (org-plantuml-jar-path . "~/.nix-profile/lib/plantuml.jar")
     ;; Do not count TODOs recursively
     (org-hierarchial-todo-statics . nil)
     ;; Do not indent source code
     (org-edit-src-content-indentation . 0)
     ;; Try `#+ATTR.*' keyword then fall back on the original width
     (org-image-actual-width . nil)
     (calendar-week-start-day . 1)
     (org-todo-keywords . '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")))
     (org-agenda-span . 7)
     (org-agenda-start-day . "+0d")
     (org-agenda-skip-timestamp-if-done . t)
     (org-agenda-skip-scheduled-if-done . t)
     (org-agenda-skip-scheduled-if-deadline-is-shown . t)
     (org-agenda-skip-timestamp-if-deadline-is-shown . t)
     (org-ellipsis . "⤵")
     (org-log-done . 'time)
     (org-src-fontify-natively . t)
     (org-use-speed-commands . t)
     (org-clock-persist . t)
     (org-time-clocksum-format . '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
     (org-tag-alist
      . '(("compe" . ?c)
          ("emacs" . ?e)
          ("life" . ?l)
          ("social" . ?s)
          ("work" . ?w)))

     ;; `https://emacs.stackexchange.com/a/17832'
     (org-agenda-prefix-format
      . '(
          ;; (agenda  . "  • ")
          (agenda  . " %i %t% s")
          ;; (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
          (timeline  . "  % s")
          (todo . " %i %-12:c")
          (tags  . " %i %-12:c")
          (search . " %i %-12:c")))
     )
    :config
    (setq org-default-notes-file (concat org-directory "/tasks.org"))

    (setq org-capture-templates
          ,(("j" "Journal" entry (file+datetree "journal.org") "* ")))

    ;; fold
    (evil-define-key 'normal org-mode-map
        "za" #'org-cycle
        "zR" #'org-fold-show-all
        ;; close/open
        "zC" #'org-fold-hide-sublevels
        "zO" #'org-fold-show-subtree
        )

    (progn ;; Setup diagram programs
        ;; Installation via `home-manager' is assumed:
        (org-babel-do-load-languages
         'org-babel-load-languages '((ditaa . t)
                                     (dot . t)
                                     (shell . t)))

        ;; (setq org-plantuml "plantuml")

        ;; FIXME: not wokring why
        ;; (org-babel-do-load-languages ‘org-babel-load-languages ‘((ditaa . t) (dot . t) (plantuml . t)))
        )

    (setq org-agenda-files
          (mapcar (lambda (path) (concat org-directory path))
                  '("/journal.org" "/timeline/")))

    ;; Start from Monday

    ;; `https://orgmode.org/manual/Breaking-Down-Tasks.html'
    (defun org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-todo-log-states)   ; turn off logging
            (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

    (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

    ;; appearance

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

    :defer-config
    (org-clock-persistence-insinuate)

    (leaf calfw)
    (leaf calfw-org
        :commands cfw:open-org-calendar)

    (defun toy/open-calendar ()
        (interactive)
        (split-window-right)
        (other-window 1)
        (cfw:open-org-calendar))

    (leaf simple-httpd
        :doc "`httpd-serve-directory' mainly for the org site"
        :config
        (defun toy/org-serve ()
            (interactive)
            (httpd-serve-directory "out")))

    (leaf ox-zenn
        :url "https://github.com/conao3/ox-zenn.el"
        :custom ((org-zenn-with-last-modified . nil)
                 (org-export-with-toc . nil))

        :config
        (defun org-zenn-export-to-markdown-as (outfile &optional async subtreep visible-only)
            (interactive "sOutfile: ")
            (org-export-to-file 'zennmd outfile async subtreep visible-only))

        (defun org-zenn-export-book-files (&optional org-dir)
            "`$org-dir' -> `$md-dir': `$zenn/books-org/$book' -> `$zenn/books/$book'"
            (interactive "sBook directory: ")
            (setq org-dir (expand-file-name (or org-dir ".")))
            (let* ((book (file-name-nondirectory org-dir))
                   (zenn (file-name-directory (directory-file-name (file-name-directory org-dir))))
                   (md-dir (concat zenn "books/" book "/")))
                (if (not (and (file-directory-p org-dir) (file-directory-p md-dir)))
                        (message "Not an org book directroy?")
                    (dolist (src-file-name (seq-filter (lambda (s) (string-suffix-p ".org" s)) (directory-files org-dir)))
                        (let* ((src-file (concat org-dir "/" src-file-name))
                               (dst-file-name (concat (file-name-base src-file-name) ".md"))
                               (dst-file (concat md-dir dst-file-name)))
                            (with-temp-buffer
                                (insert-file-contents src-file)
                                (org-zenn-export-to-markdown-as dst-file nil nil nil)))))))

        (defun org-zenn-export-buffer-to-book (&optional org-dir)
            "Runs subtree export to each level 1 headings"
            (interactive)
            (org-map-entries
             (lambda ()
                 (org-zenn-export-to-markdown nil t))
             "LEVEL=1")))

    (leaf org-appear
        :doc "Uninline format on cursor"
        :url "https://github.com/awth13/org-appear"
        :hook (org-mode-hook . org-appear-mode)
        :custom
        (org-appear-autolinks . t))

    (setq org-hide-emphasis-markers t)

    (leaf org-superstar
        :commands org-superstar-mode
        :hook (org-mode-hook . org-superstar-mode)
        :custom
        (org-superstar-special-todo-items . nil))

    ;; html view
    (leaf org-preview-html
        :commands org-preview-html-mode org-preview-html/preview)

    ;; C-c C-j: create entry
    (leaf org-journal
        :custom ((org-journal-dir . "~/org-priv/journal/")
                 (org-journal-date-format . "%Y-%m-%d")))

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
        " oz" #'org-zenn-export-buffer-to-book)

    (leaf org-roam
        :custom
        ;; any effect?
        ((org-roam-v2-ack . t)
         (org-roam-directory . "~/org-roam")
         ;; (org-refile-targets
         ;;   .   '(("~/org/gtd/gtd.org" :maxlevel . 3)
         ;;         ("~/org/gtd/someday.org" :level . 1)
         ;;         ("~/org/gtd/tickler.org" :maxlevel . 2)))
         )
        :config
        (org-roam-db-autosync-mode)

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

