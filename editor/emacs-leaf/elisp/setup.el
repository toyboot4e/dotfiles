;; -*- lexical-binding: t; -*-

(setq user-full-name    "toyboot4e"
      user-mail-address "toyboot4e@gmail.com")

;; --------------------------------------------------------------------------------
;; Boostrapping

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
        (leaf-keywords-init))
    )
;; </leaf-install-code>


