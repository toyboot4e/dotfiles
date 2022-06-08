;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration based on `init.el' and `evil.el'

;;; Code:

(setq user-full-name    "toyboot4e"
      user-mail-address "toyboot4e@gmail.com")

(when (version< emacs-version "27.1") (error "Update your Emacs!"))

;;; Init files

(setq toy/init-files
      '("elisp/gc.el"                    ;; GC settings for startup speed
        "elisp/conf.el"                  ;; Basic configurations
        "elisp/managed.el"               ;; ELisp files managed with `leaf-manager'. Most packages are in it.
        "elisp/ide-consult.el"           ;; Consult
        "elisp/org.el"                   ;; org-mode
        "elisp/web.el"                   ;; Web support
        "elisp/hydra.el"                 ;; Hydras
        "elisp/keymap.el"                ;; Key mappings
        "elisp/keymap-consult.el"        ;; Key mappings
        ))

;;; Boostrapping

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
                         ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize)
    (unless (package-installed-p 'leaf)
        (package-refresh-contents)
        (package-install 'leaf))

    ;; Always `ensure t'
    (leaf leaf
        :custom ((leaf-defaults . '(:ensure t))))

    (leaf leaf-keywords
        :init
        (leaf feather
            :doc "Parallel thread modern package manager"
            :url "https://github.com/conao3/feather.el"
            :req "emacs-26.3" "async-await-1.0" "ppp-1.0" "page-break-lines-0.1"
            :tag "convenience" "package" "emacs>=26.3"
            :emacs>= 26.3
            :ensure t)

        :config
        (leaf-keywords-init)))
;; </leaf-install-code>

;;; Preferences

;; $ brew install emacs-plus --with-no-titlebar
(setq frame-resize-pixelwise t)

;; "Package 'cl is deprecated".. I know!
(setq byte-compile-warnings '(not cl-functions obsolete))

;;; User environment

;; Set up `PATH` and `exec-path`
(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/opt/local/bin" "/sw/bin"
                   "~/.cargo/bin"
                   ;; "~/.idris2/bin"
                   "~/.nix-profile/bin"
                   "/usr/local/bin"
                   "~/bin"
                   ;; Unforunate path to LaTeX on my mac
                   "/Library/TeX/texbin"
                   "/usr/local/texlive/2019/bin/x86_64-darwin/"
                   ))

    (when (and (file-exists-p dir) (not (member dir exec-path)))
        (setenv "PATH" (concat dir ":" (getenv "PATH")))
        (setq exec-path (append (list dir) exec-path))))

;;; Meta utilities

(defmacro _fn (&rest body)
    "Shorthand for interactive lambda."
    (declare (doc-string 1))
    `(lambda () (interactive) ,@body))

;;; Loading

(setq vc-follow-symlinks t)

(dolist (x toy/init-files)
    (load-file (concat user-emacs-directory x))

    ;; lighter magit
    (defun magit-rev-format (format &optional rev args)
        (let ((str (magit-git-string "log" "-1" "--no-patch"
                                     (concat "--format=" format) args
                                     (if rev (concat rev "^{commit}") "HEAD") "--")))
            (unless (string-equal str "")
                str))))

;;; Theme

(defun toy/setup-theme ()
    (leaf doom-themes
        :config
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t
              doom-themes-enable-italic t)
        ;; flush mode-line on errors
        ;; (doom-themes-visual-bell-config)

        (load-theme 'doom-opera t)

        ;; `smyx' is an old package, so first load doom theme and then overwrite most colors
        (leaf smyx-theme
            :straight (smyx-theme :type git :host github :repo "tacit7/smyx")
            :config
            (load-theme 'smyx t))
        )
    )

;;; On startup

(defun toy/on-start ()
    ;; Like Vim, when multiple files are opened on startup,
    ;; we will focus the first one and close other windows.
    (delete-other-windows)
    (toy/setup-theme))

(add-hook 'window-setup-hook #'toy/on-start)

;;; init.el ends here
