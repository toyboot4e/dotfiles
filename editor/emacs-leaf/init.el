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
        "elisp/delayed.el"               ;; TODO
        ))

(dolist (x toy/init-files)
    (load-file (concat user-emacs-directory x)))

(setq toy/delayed-init-files
      '(
        "elisp/managed.el"               ;; ELisp files managed with `leaf-manager'
        "elisp/conf.el"                  ;; Basic configurations
        "local/locals.el"                ;; Local packages
        "elisp/ide-consult.el"           ;; Consult
        ;; "elisp/org.el"                   ;; org-mode
        ;; "elisp/web.el"                   ;; Web support
        "elisp/hydra.el"                 ;; Hydra-based key mapigns
        "elisp/keymap.el"                ;; Key mappings
        "elisp/keymap-consult.el"        ;; Key mappings
        "elisp/end.el"                   ;; Run after startup
        ))

(dolist (x toy/delayed-init-files)
    (load-file (concat user-emacs-directory x)))

;; (progn
;;     (toy/setup-delayed-tasks)
;;     (dolist (x toy/delayed-init-files)
;;         (let ((f (concat user-emacs-directory x)))
;;             (load-delayed f)))
;;     (toy/run-delayed-tasks))

;; TODO: Compile
;; (byte-recompile-directory (concat user-emacs-directory "elisp"))

;; ;; TODO: Load `.elc` version if it exists
;; (dolist (x toy/init-files)
;;     (let ((path (replace-regexp-in-string "\.el$" ".elc" x)))
;;         (message path)
;;         (load-file (concat user-emacs-directory path))))

;;; init.el ends here
