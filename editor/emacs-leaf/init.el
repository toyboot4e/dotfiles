;; -*- lexical-binding: t -*-

;; NOTE: Open / source `init.el' with `:ed' and `:s' (defined with `leaf evil')
;; NOTE: Run `:exit-minibuffer' (Spc-Spc-r) when `d' motion went wrong (Evil).

(when (version< emacs-version "27.1") (error "Update your Emacs!"))
(setq vc-follow-symlinks t)

;; what?: `https://github.com/doomemacs/doomemacs/issues/5682'
(defvar native-comp-deferred-compilation-deny-list nil)

;; "tempel-snippets.el"

(setq toy/init-files
      '(
        "elisp/env.el"                   ;; User environment and preferences
        "elisp/early-conf.el"            ;; No-deps configuration
        "elisp/setup.el"                 ;; Bootstrapping

        "locals/locals.el"               ;; Local packages
        "elisp/conf.el"                  ;; Basic configurations
        "elisp/managed.el"               ;; ELisp files managed with `leaf-manager'
        "elisp/minadwares.el"            ;; minad packages
        ;; "elisp/ide-dap.el"               ;; dap-mode

        "elisp/org.el"                   ;; org-mode
        ;; "elisp/web.el"                   ;; Web support

        "elisp/hydra.el"                 ;; Hydra-based key mappigns
        "elisp/keymap.el"                ;; Key mappings
        "elisp/keymap-consult.el"        ;; Key mappings

        "elisp/end.el"                   ;; Run after startup
        ))

(defun toy/load-sub-config ()
    (interactive)
    (dolist (x toy/init-files)
        (load-file (concat user-emacs-directory x))))

(defun toy/reload ()
    (interactive)
    (load-file (concat user-emacs-directory "init.el")))

(defun toy/on-startup ()
    (toy/load-sub-config)
    (toy/on-start))

;; TODO: delay most code load

(toy/load-sub-config)
(add-hook 'window-setup-hook #'toy/on-start)

;;; init.el ends here
