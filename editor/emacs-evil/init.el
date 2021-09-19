(setq user-full-name    "toyboot4e"
      user-mail-address "toyboot4e@gmail.com")

(when (version< emacs-version "27.1") (error "Update your Emacs!"))

;; brew install emacs-plus --with-no-titlebar
(setq frame-resize-pixelwise t)

(progn
    ;; FIX to Evil
    ;; https://emacs.stackexchange.com/questions/35946/strange-behaviour-on-evil-delete
    (defun toy/stop-using-minibuffer ()
        "kill the minibuffer"
        (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
            (abort-recursive-edit)))

    (add-hook 'mouse-leave-buffer-hook 'toy/stop-using-minibuffer))

;; "Package 'cl is deprecated".. I know!
(setq byte-compile-warnings '(not cl-functions obsolete))

;; read symlinks
(setq vc-follow-symlinks t)

(defmacro _fn (&rest body)
    "Shorthand for interactive lambda."
    (declare (doc-string 1))
    `(lambda () (interactive) ,@body))

;; files to load
(setq toy/init-files
      '("elisp/gc.el"                    ;; GC settings for startup speed

        ;; packages
        "elisp/evil.el"                  ;; Fundamentals
        "local/locals.el"                ;; Local packages

        "elisp/ide-ui.el"                ;; Intelligence
        "elisp/ide-langs.el"             ;; Intelligence for specific laguages
        "elisp/ide-consult.el"           ;; Intelligence
        "elisp/markup.el"                ;; Tiny language supports
        "elisp/web.el"                   ;; Web support

        ;; configuration
        "elisp/hydra.el"                 ;; Hydras
        "elisp/keymap.el"                ;; Key mappings
        "elisp/keymap-consult.el"        ;; Key mappings

        "elisp/private.el"               ;; Private code, like API tokens
        ))

(defun toy/fill-mode ()
    (interactive)
    (set-fill-column 100)
    (auto-fill-mode))

;; Add `local/` to `load-path` recursively
;; https://www.emacswiki.org/emacs/LoadPath
(progn
    (add-to-list 'load-path (concat user-emacs-directory "local"))
    (let ((default-directory  (concat user-emacs-directory "local")))
        (normal-top-level-add-subdirs-to-load-path))
    )

;; Load all of the listed files
(dolist (x toy/init-files)
    (load-file (concat user-emacs-directory x)))

;; start Emacs with only one window
(add-hook 'window-setup-hook #'delete-other-windows)

