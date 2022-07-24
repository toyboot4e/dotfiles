;; -*- lexical-binding: t -*-

;; NOTE: Open / source `init.el' with `:ed' and `:s'!

(when (version< emacs-version "27.1") (error "Update your Emacs!"))

;; Profiling
;; (load-file (concat user-emacs-directory "elisp/profile.el"))

(setq vc-follow-symlinks t)

(setq toy/init-files
      '(
        "elisp/setup.el"                 ;; Boostrapping

        "elisp/env.el"                   ;; User environment and preferences
        "local/locals.el"                ;; Local packages

        "elisp/managed.el"               ;; ELisp files managed with `leaf-manager'
        "elisp/conf.el"                  ;; Basic configurations
        "elisp/ide-consult.el"           ;; Consult
        ;; "elisp/ide-dap.el"               ;; dap-mode

        ;; "elisp/org.el"                   ;; org-mode
        ;; "elisp/web.el"                   ;; Web support

        "elisp/hydra.el"                 ;; Hydra-based key mapigns
        "elisp/keymap.el"                ;; Key mappings
        "elisp/keymap-consult.el"        ;; Key mappings

        "elisp/end.el"                   ;; Run after startup
        ))

(dolist (x toy/init-files)
    (let ((url (concat user-emacs-directory x)))
        (message url)
        (load-file url)))
    ;; (load-file (concat user-emacs-directory x)))

;;; init.el ends here
