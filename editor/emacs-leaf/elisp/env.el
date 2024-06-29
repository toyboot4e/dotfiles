;; -*- lexical-binding: t -*-

;; --------------------------------------------------------------------------------
;; User environment

;; Set up `PATH` and `exec-path`
(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/opt/local/bin" "/sw/bin"
                   "~/.cargo/bin"
                   ;; "~/.idris2/bin"
                   "~/.nix-profile/bin"
                   "/usr/local/bin"
                   "~/bin"
                   ))

    (when (and (file-exists-p dir) (not (member dir exec-path)))
        (setenv "PATH" (concat dir ":" (getenv "PATH")))
        (setq exec-path (append (list dir) exec-path))))

;; --------------------------------------------------------------------------------
;; Preferences

;; $ brew install emacs-plus --with-no-titlebar
(setq frame-resize-pixelwise t)

;; "Package 'cl is deprecated".. I know!
(setq byte-compile-warnings '(not cl-functions obsolete))

;; --------------------------------------------------------------------------------
;; Meta utilities

(defmacro _fn (&rest body)
    "Shorthand for interactive lambda."
    (declare (doc-string 1))
    `(lambda () (interactive) ,@body))

