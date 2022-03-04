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
                    "/dev-ink.org"
                    "/agenda.org"
                    "/bevy.org"
                    "/read.org"
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
    "z0" #'evil-open-folds)

