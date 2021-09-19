;; consult keymap  -*- lexical-binding: t -*-

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

(progn ;; keymap for `consult-find-map'
    ;; TODO: use it
    (defun toy/consult-backspace ()
        "Go up directory."
        (interactive)
        (let ((c (char-before (point))))
            (if (= c ?/)
                    (backward-delete-char 1)
                (let* ((p (point))
                       (p1 (progn (skip-chars-backward "^/")
                                  (point)))
                       (p2 (progn (skip-chars-forward "^/")
                                  (if (= (point-at-eol) (point))
                                          (point)
                                      (+ 1 (point))))))
                    (kill-region p1 p2)))))

    ;; (defvar toy/consult-find-map
    ;;     (let ((map (make-sparse-keymap)))
    ;;         (define-key map (kbd "<DEL>") #'previous-history-element)
    ;;         map))

    ;; (consult-customize consult-find :keymap toy/consult-find-map)
    )

;; TODO: kill with C-k

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

;; TODO: why do we have to use `define-key`?
(define-key minibuffer-local-map (kbd "\C-a") #'evil-first-non-blank)
(define-key minibuffer-local-map (kbd "\C-e") #'end-of-line)

(define-key minibuffer-local-map (kbd "\C-f") #'evil-scroll-up)
(define-key vertico-map (kbd "\C-f") #'vertico-scroll-up)

(define-key minibuffer-local-map (kbd "\C-b") #'evil-scroll-down)
(define-key vertico-map (kbd "\C-b") #'vertico-scroll-down)

