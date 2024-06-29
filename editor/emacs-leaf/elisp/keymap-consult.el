;; -*- lexical-binding: t -*-

;; consult keymap

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
    (consult-fd (projectile-project-root) ". "))

(defun toy/proj-grep ()
    "Find a file from project files."
    (interactive)
    (consult-ripgrep (projectile-project-root) " . "))

(evil-define-key 'normal 'toy/global-mode-map
    " v" (lambda () (interactive) (when (toy/vf) (toy/force-center)))
    " V" (lambda () (interactive) (when (toy/vF) (toy/force-center)))

    " ff" (_cd (call-interactively #'find-file))
    " fF" #'toy/proj-find
    " fb" #'consult-buffer
    " fB" #'consult-buffer-other-window
    ;; " fB" #'org-switchb

    " fr" #'consult-recentf
    " fh" #'consult-history

    " fl" #'consult-line
    " fo" #'consult-outline
    " fO" (lambda () (interactive) (cd org-directory) (call-interactively #'find-file))

    " fi" #'consult-imenu
    " fd" #'consult-lsp-diagnostics
    " fs" #'consult-lsp-symbols

    ;; " fp" (lambda () (interactive) (call-interactively #'projectile-invalidate-cache) (projectile-switch-project))
    " fp" #'projectile-switch-project
    " fG" #'consult-ghq-find
    " fD" #'consult-dir
    " fE" #'consult-flycheck

    ;; evil
    ;; " fm" #'consult-evil-marks
    ;; " fr" #'consult-evil-registers
    " fm" #'consult-mark
    " fM" #'consult-global-mark
    " fr" #'consult-register
    )

;; [g]rep
(evil-define-key 'normal 'toy/global-mode-map
    " gb" #'consult-line
    ;; in all buffer
    ;; " gB" #'
    " gr" (lambda () (interactive) (consult-grep "." " . "))
    " gR" #'toy/proj-grep)

(define-key minibuffer-local-map (kbd "\C-a") #'evil-first-non-blank)
(define-key minibuffer-local-map (kbd "\C-e") #'end-of-line)
(define-key minibuffer-local-map (kbd "\C-r") #'evil-paste-from-register)

(with-eval-after-load 'vertico
    ;; TODO: recenter-top-bottom for vertico buffer
    (define-key vertico-map (kbd "\C-o") #'other-window)
    (define-key vertico-map (kbd "\C-u") #'vertico-scroll-down)
    (define-key vertico-map (kbd "\C-d") #'vertico-scroll-up)
    ;; TODO: use isearch forward?
    (define-key vertico-map (kbd "\C-r") #'evil-paste-from-register))

