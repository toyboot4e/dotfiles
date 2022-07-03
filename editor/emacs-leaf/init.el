;; -*- lexical-binding: t -*-

(when (version< emacs-version "27.1") (error "Update your Emacs!"))

;; Profiling
;; (load-file (concat user-emacs-directory "elisp/profile.el"))

(setq toy/init-files
      '(
        "elisp/setup.el"                 ;; Boostrapping
        "elisp/env.el"                   ;; User environment and preferences
        "elisp/conf.el"                  ;; Basic configurations
        "local/locals.el"                ;; Local packages
        "elisp/managed.el"               ;; ELisp files managed with `leaf-manager'
        "elisp/ide-consult.el"           ;; Consult
        "elisp/org.el"                   ;; org-mode
        "elisp/web.el"                   ;; Web support
        "elisp/keymap.el"                ;; Key mappings
        "elisp/keymap-consult.el"        ;; Key mappings
        "elisp/end.el"                   ;; Run after startup
        ))

(setq vc-follow-symlinks t)

(dolist (x toy/init-files)
    (load-file (concat user-emacs-directory x)))

;;; init.el ends here
