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
                   ;; Unforunate path to LaTeX on my mac
                   "/Library/TeX/texbin"
                   "/usr/local/texlive/2019/bin/x86_64-darwin/"
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

;; lighter magit
(defun magit-rev-format (format &optional rev args)
    (let ((str (magit-git-string "log" "-1" "--no-patch"
                                 (concat "--format=" format) args
                                 (if rev (concat rev "^{commit}") "HEAD") "--")))
        (unless (string-equal str "")
            str)))

;; --------------------------------------------------------------------------------
;; Meta utilities

(defmacro _fn (&rest body)
    "Shorthand for interactive lambda."
    (declare (doc-string 1))
    `(lambda () (interactive) ,@body))

