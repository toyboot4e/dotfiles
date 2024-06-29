;; -*- lexical-binding: t -*-

;; NOTE: Open / source `init.el' with `:ed' and `:s' (defined with `leaf evil')
;; NOTE: Run `:exit-minibuffer' (Spc-Spc-r) when `d' motion went wrong (Evil).

(when (version< emacs-version "27.1") (error "Update your Emacs!"))
(setq vc-follow-symlinks t)

;; "tempel-snippets.el"

(setq toy/init-files
      '(
        "elisp/env.el"                   ;; User environment and preferences
        "elisp/early-conf.el"            ;; No-deps configuration
        "elisp/setup.el"                 ;; Bootstrapping + no-littering

        "locals/locals.el"               ;; Local packages (if any)
        "elisp/conf.el"                  ;; Basic configurations
        "elisp/managed.el"               ;; ELisp files managed with `leaf-manager'
        "elisp/hacks.el"                 ;; Dirty fix ELisp
        "elisp/minadwares.el"            ;; minad packages
        ;; "elisp/ide-dap.el"               ;; dap-mode

        "elisp/org.el"                   ;; org-mode
        ;; "elisp/web.el"                   ;; Web support

        "elisp/hydra.el"                 ;; Hydra-based key mappigns
        "elisp/keymap.el"                ;; Key mappings
        "elisp/keymap-consult.el"        ;; Key mappings
        ;; "elisp/ddskk-conf.el"            ;; IME

        "elisp/end.el"                   ;; Run after startup
        ))

(defun toy/load-configuration ()
    (interactive)
    (dolist (x toy/init-files)
        (load-file (concat user-emacs-directory x))))

(defun toy/reload ()
    (interactive)
    (load-file (concat user-emacs-directory "init.el")))

(toy/load-configuration)

;;; init.el ends here
