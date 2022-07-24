;; -*- lexical-binding: t -*-

;; consult keymap

;; https://github.com/minad/consult/wiki
(defun define-minibuffer-key (key &rest defs)
    "Define KEY conditionally in the minibuffer.
DEFS is a plist associating completion categories to commands."
    (define-key minibuffer-local-map key
        (list 'menu-item nil defs :filter
              (lambda (d)
                  (plist-get d (completion-metadata-get
                                (completion-metadata (minibuffer-contents)
                                                     minibuffer-completion-table
                                                     minibuffer-completion-predicate)
                                'category))))))

(progn ;; ???
    (defun consult-find-for-minibuffer ()
        "Search file with find, enter the result in the minibuffer."
        (interactive)
        (let* ((enable-recursive-minibuffers t)
               (default-directory (file-name-directory (minibuffer-contents)))
               (file (consult--find
                      (replace-regexp-in-string
                       "\\s-*[:([].*"
                       (format " (via find in %s): " default-directory)
                       (minibuffer-prompt))
                      consult-find-command
                      (file-name-nondirectory (minibuffer-contents)))))
            (delete-minibuffer-contents)
            (insert (expand-file-name file default-directory))
            (exit-minibuffer)))

    (define-minibuffer-key "\C-s"
        'consult-location #'previous-history-element
        'file #'consult-find-for-minibuffer)
    )

;; FIXME: just use let?
(defmacro _cd (&rest body)
    "Call consult function with `default-directory' as root directory."
    (declare (doc-string 1))
    `(lambda () (interactive)
         (let ((consult-project-root-functiom nil))
             ,@body
             )))

(evil-define-key 'normal 'toy/global-mode-map
    " :" #'execute-extended-command
    )

(defun toy/proj-find ()
    "Find a file from project files."
    (interactive)
    (affe-find (projectile-project-root) ". "))

(defun toy/proj-grep ()
    "Find a file from project files."
    (interactive)
    (affe-grep (projectile-project-root) ""))

(evil-define-key 'normal 'toy/global-mode-map
    " v" (lambda () (interactive) (when (toy/vf) (toy/force-center)))
    " V" (lambda () (interactive) (when (toy/vF) (toy/force-center)))

    " ff" (_cd (call-interactively #'find-file))
    " fF" #'toy/proj-find
    " fb" #'consult-buffer
    " fB" #'consult-buffer-other-window

    " fr" #'consult-recentf
    " fh" #'consult-history
    " fM" #'consult-mode-command

    " fl" #'consult-line
    " fo" #'consult-outline
    " fO" #'org-switchb

    " fi" #'consult-imenu
    " fd" #'consult-lsp-diagnostics
    " fs" #'consult-lsp-symbols

    " fp" #'projectile-switch-project
    " fG" #'consult-ghq-find
    " fD" #'consult-dir

    ;; evil
    ;; " fm" #'consult-evil-marks
    ;; " fr" #'consult-evil-registers
    " fm" #'consult-mark
    " fr" #'consult-register
    )

;; [g]rep
(evil-define-key 'normal 'toy/global-mode-map
    " gb" #'consult-line
    ;; in all buffer
    ;; " gB" #'
    " gr" #'affe-grep
    " gR" #'toy/proj-grep
    )

(define-key minibuffer-local-map (kbd "\C-a") #'evil-first-non-blank)
(define-key minibuffer-local-map (kbd "\C-e") #'end-of-line)

(with-eval-after-load 'vertico
    ;; TODO: recenter-top-bottom for vertico buffer
    (define-key vertico-map (kbd "\C-u") (lambda () (interactive) (vertico-scroll-down 1)))
    (define-key vertico-map (kbd "\C-d") (lambda () (interactive) (vertico-scroll-up 1)))

    ;; (define-key vertico-map (kbd "\C-d") #'vertico-next-group)
    ;; (define-key vertico-map (kbd "\C-u") #'vertico-previous-group)
    )


