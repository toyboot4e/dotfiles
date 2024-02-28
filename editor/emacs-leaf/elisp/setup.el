;; -*- lexical-binding: t; -*-

(setq user-full-name    "toyboot4e"
      user-mail-address "toyboot4e@gmail.com")

;; --------------------------------------------------------------------------------
;; Bootstrapping

;; TODO: Just use `use-package' for bootstrapping?

(setq straight-vc-git-default-protocol 'ssh)

(progn ;; `straight.el'
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
        (unless (file-exists-p bootstrap-file)
            (with-current-buffer
                    (url-retrieve-synchronously
                     "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                     'silent 'inhibit-cookies)
                (goto-char (point-max))
                (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage)))

;; <leaf-install-code>
;; `leaf.el'
(eval-and-compile
    (customize-set-variable
     'package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
    (package-initialize)
    (unless (package-installed-p 'leaf)
        (package-refresh-contents)
        (package-install 'leaf))

    ;; Always `ensure t'
    (leaf leaf
        :custom ((leaf-defaults . '(:ensure t))))

    (leaf leaf-keywords
        :config
        (leaf-keywords-init)))
;; </leaf-install-code>

;; (leaf setup
;;     :doc "Helpful Configuration Macro"
;;     :straigh (setup :type git :host github :repo "github.com/zk-phi/setup"))

;; --------------------------------------------------------------------------------
;; Meta
;; --------------------------------------------------------------------------------

(leaf no-littering
    :url "https://github.com/emacscollective/no-littering"
    :init
    (setq no-littering-etc-directory
          (expand-file-name "etc/" user-emacs-directory))
    (setq no-littering-var-directory
          (expand-file-name "var/" user-emacs-directory))
    :config
    (require 'recentf)
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory))
    (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

