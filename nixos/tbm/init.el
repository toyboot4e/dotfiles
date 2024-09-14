;; -*- lexical-binding: t -*-

(setopt user-full-name    "toyboot4e"
        user-mail-address "toyboot4e@gmail.com")

(when (version< emacs-version "27.1") (error "Update your Emacs!"))
(setopt vc-follow-symlinks t)

(progn ;; disable magic file name on startup for a little bit faster start
    (defconst my-saved-file-name-handler-alist file-name-handler-alist)
    (setopt file-name-handler-alist nil))

;; FIXME: emacs-init-time is lying??
(defun toy/after-startup ()
    (message "Emacs ready in %s with %d garbage collections."
             (emacs-init-time) gcs-done)
    ;; re-enable magic file name
    (setopt file-name-handler-alist my-saved-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'toy/after-startup)

(defun toy/reload ()
    "tangle and load `init.org'"
    (interactive)
    (require 'ob-tangle)
    (org-babel-tangle-file (concat user-emacs-directory "init.org"))
    (load-file (concat user-emacs-directory "init.el")))

;; $ brew install emacs-plus --with-no-titlebar
(setopt frame-resize-pixelwise t)

(defmacro _fn (&rest body)
    "Shorthand for interactive lambda."
    (declare (doc-string 1))
    `(lambda () (interactive) ,@body))

;; TODO: save backup files, but in other directory:
(setopt make-backup-files nil)
(setopt backup-directory-alist
      `(("." . ,(expand-file-name "backup/" user-emacs-directory))))

(setopt lock-file-name-transforms
      `(("\\`/.*/\\([^/]+\\)\\'" ,(expand-file-name "backup/\\1" user-emacs-directory) t)))

;; don't create #autosave# files
(setopt auto-save-default nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setopt initial-scratch-message "")

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setopt locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
;; it modifies the buffer
;; (set-buffer-file-coding-system 'utf-8)
;; it requires flusing
;; (set-terminal-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; (setopt straight-vc-git-default-protocol 'ssh)
;; 
;; (progn ;; `straight.el'
;;     (defvar bootstrap-version)
;;     (let ((bootstrap-file
;;            (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;           (bootstrap-version 5))
;;         (unless (file-exists-p bootstrap-file)
;;             (with-current-buffer
;;                     (url-retrieve-synchronously
;;                      "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;                      'silent 'inhibit-cookies)
;;                 (goto-char (point-max))
;;                 (eval-print-last-sexp)))
;;         (load bootstrap-file nil 'nomessage)))

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
        ;; FIXME: the custom value should be after `no-jittering'
        :custom ((leaf-defaults . '(:ensure t))))

    ;; Fixme: permissoion-denied on nixos-rebuild?
    (leaf leaf-keywords
        :config
        (leaf-keywords-init)))

;; (leaf exec-path-from-shell
;;     :url "https://github.com/purcell/exec-path-from-shell"
;;     :custom
;;     ((exec-path-from-shell-shell-name . "sh")
;;      (exec-path-from-shell-variables . '("PATH"
;;                                          "GOROOT"
;;                                          "GOPATH"
;;                                          "LSP_USE_PLISTS"
;;                                          "TERM")))
;;     :config
;;     (exec-path-from-shell-initialize))

;; Set up `PATH` and `exec-path`
(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/opt/local/bin" "/sw/bin"
                   "~/.cargo/bin"
                   ;; "~/.idris2/bin"
                   "~/.nix-profile/bin"
                   "/usr/local/bin"
                   "~/bin"
                   "/run/current-system/sw/bin"
                   ))
    (when (and (file-exists-p dir) (not (member dir exec-path)))
        (setenv "PATH" (concat dir ":" (getenv "PATH")))
        (setopt exec-path (append (list dir) exec-path))))

(leaf no-littering
    :url "https://github.com/emacscollective/no-littering"
    :init
    ;; default.el:139:44: Warning: reference to free variable `no-littering-var-directory'
    ;; default.el:138:19: Warning: reference to free variable `recentf-exclude'
    (setopt no-littering-etc-directory
            (expand-file-name "etc/" user-emacs-directory))
    (setopt no-littering-var-directory
            (expand-file-name "var/" user-emacs-directory))
    :config
    (require 'recentf)
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory))
    (setopt custom-file (no-littering-expand-etc-file-name "custom.el")))

;; show line numbers
(global-display-line-numbers-mode)

;; highlight current line
;; (global-hl-line-mode t)

;; show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; show tabs
(require 'whitespace)
(whitespace-mode 1)
(setopt whitespace-style '(tabs  tab-mark))

;; show matching parentheses
(setq-default show-paren-delay 0)
(show-paren-mode 1)

;; show `line:column` in the modeline
(column-number-mode)

(set-cursor-color "#8fee96")
(set-fringe-mode 10)

(when (display-graphic-p)
    ;; (set-face-attribute 'default nil :family "roboto-mono" :height 110)
    ;; (set-face-attribute 'default nil :family "roboto-mono")
    ;; (set-face-attribute 'default nil :family "Noto Sans Mono")

    ;; TODO: setup monospaced font
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      ;; TODO: fallback
                      ;; (font-spec :family "Hiragino Kaku Gothic ProN")
                      (font-spec :family "Noto Sans Mono CJK JP"))

    ;; FIXME: proper way to align org tables?
    (setopt face-font-rescale-alist
            '(("Noto Sans Mono CJK JP" . 1.25))))

(unless (display-graphic-p)
    ;; Two exclusive options:
    ;; 1. use left click to move cursor:
    (xterm-mouse-mode 1)

    ;; 2. use left click to select (and copy):
    ;; (xterm-mouse-mode -1)

    ;; Use mouse wheel for scrolling
    ;; TODO: do not map it to a specific command.
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(setopt scroll-preserve-screen-position t
        scroll-conservatively 100
        scroll-margin 3)

(setopt history-length 1000
        history-delete-duplicates t)
(savehist-mode)

(setopt auto-revert-interval 1)
(global-auto-revert-mode)

(save-place-mode 1)

(defun toy/fix-save-place ()
    "Force windows to recenter current line (with saved position)."
    (run-with-timer 0 nil
                    (lambda (buf)
                        (when (buffer-live-p buf)
                            (dolist (win (get-buffer-window-list buf nil t))
                                (with-selected-window win (recenter)))))
                    (current-buffer)))
(add-hook 'find-file-hook #'toy/fix-save-place)

(setopt recentf-max-saved-items 1000)
(recentf-mode 1)

(setopt uniquify-buffer-name-style 'post-forward-angle-brackets)
(require 'uniquify)

(leaf evil
    :leaf-defer nil
    :commands evil-define-key
    :custom (;; free `z` for background
             (evil-toggle-key . "")
             ;; for `evil-collection'
             (evil-want-keybinding . nil)
             (evil-want-minibuffer . nil)
             ;; (evil-want-C-u-delete . t)
             (evil-want-C-u-scroll . t)
             (evil-want-C-d-scroll . t)
             (evil-want-Y-yank-to-eol . t)
             ;; else
             (evil-move-cursor-back . t)
             (evil-search-module quote evil-search))

    :config
    (evil-mode 1))

;; (leaf empv
;;     :ensure nil
;;     :after evil
;;     :straight (empv :type git :host github :repo "isamert/empv.el"))

(leaf undo-tree
    :custom (undo-tree-auto-save-history . nil)
    :init
    (evil-set-undo-system 'undo-tree)
    (global-undo-tree-mode))

(leaf evil-anzu
    :after evil
    :url "https://github.com/emacsorphanage/evil-anzu"
    ;; :commands "anzu-query-replace-regexp"
    )

(leaf evil-surround
    :after evil
    :config
    ;; FIXME: deduplicate
    ;; (setq-default evil-surround-pairs-alist
    ;;               (push '(?~ . ("==" . "==")) evil-surround-pairs-alist))
    ;; this macro was copied from here: https://stackoverflow.com/a/22418983/4921402
    (defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
      (let ((inner-name (make-symbol (concat "evil-inner-" name)))
            (outer-name (make-symbol (concat "evil-a-" name))))
        `(progn
           (evil-define-text-object ,inner-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count nil))
           (evil-define-text-object ,outer-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count t))
           (define-key evil-inner-text-objects-map ,key #',inner-name)
           (define-key evil-outer-text-objects-map ,key #',outer-name))))
    
    (define-and-bind-quoted-text-object "pipe" "|" "|" "|")
    (define-and-bind-quoted-text-object "slash" "/" "/" "/")
    (define-and-bind-quoted-text-object "asterisk" "*" "*" "*")
    (define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$") ;; sometimes your have to escape the regex
    (define-and-bind-quoted-text-object "equals" "=" "=" "=")

    (global-evil-surround-mode))

(leaf evil-embrace
    :url "https://github.com/cute-jumper/evil-embrace.el"
    :doc "more to `evil-surround'"
    :custom
    (evil-embrace-evil-surround-keys . '(?\| ?\/ ?\* ?\= ?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?< ?> ?b ?B ?t ?\C-\[ ?w ?W ?s ?p))
    ;;                                    ~~~~~~~~~~~~~~~ added. TODO: use push, but deduplicated
    :config
    ?\| ?\/ ?\* ?\=
    (evil-embrace-enable-evil-surround-integration))

(leaf expand-region
    :after evil
    :config
    (evil-define-key 'visual 'global "v" #'er/expand-region "V" #'er/contract-region))

(leaf evil-collection
    :after evil
    :leaf-defer nil
    :commands evil-collection-init
    :custom (evil-collection-magit-use-z-for-folds . t)
    :config
    (evil-collection-init
     '(calendar
       consult
       corfu
       dired
       doc-view
       elfeed
       elisp-mode
       ;; neotree
       embark
       eww
       forge
       info
       magit
       markdown-mode
       minibuffer-mode
       org
       org-roam
       pdf
       slime
       sly
       w3m))

    ;; FIXME: `evil-collection' bug?
    (with-eval-after-load 'org
        (evil-define-key 'motion 'evil-org-mode "d" 'evil-delete)
        (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle))

    (with-eval-after-load 'magit
        (evil-define-key 'normal magit-mode-map
            "zz" #'evil-scroll-line-to-center
            "z-" #'evil-scroll-line-to-bottom
            "za" #'magit-section-toggle
            (kbd "Tab") #'magit-section-toggle
            (kbd "z RET")
            #'evil-scroll-line-to-top
            (kbd "SPC RET")
            #'magit-diff-visit-worktree-file-other-window)
        (evil-define-key 'normal git-rebase-mode-map "C-j" git-rebase-move-line-down "C-u" git-rebase-move-line-up)
        (advice-add 'magit-section-forward :after
                    (lambda (&rest _)
                        (evil-scroll-line-to-top
                         (line-number-at-pos))))
        (advice-add 'magit-section-backward :after
                    (lambda (&rest _)
                        (evil-scroll-line-to-top
                         (line-number-at-pos))))
        (advice-add 'magit-section-forward-sibling :after
                    (lambda (&rest _)
                        (evil-scroll-line-to-top
                         (line-number-at-pos))))
        (advice-add 'magit-section-backward-sibling :after
                    (lambda (&rest _)
                        (evil-scroll-line-to-top
                         (line-number-at-pos))))
        (evil-collection-magit-setup)))

(leaf evil-exchange
    :doc "Use `gx' + text object for swapping"
    :url "https://github.com/Dewdrops/evil-exchange"
    :after evil
    :config
    (evil-exchange-install))

(leaf evil-args
    :doc "Add `a'rgument text object"
    :config
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    ;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)
    )

;; (leaf evil-textobj-tree-sitter)

(leaf evil-lion
    :doc "Add `gl' and `gL' algin operators"
    :url "https://github.com/edkolev/evil-lion"
    :after evil
    :config
    (evil-define-key 'normal 'toy/global-mode-map "gl" #'evil-lion-left "gL" #'evil-lion-right)
    (evil-define-key 'visual 'toy/global-mode-map "gl" #'evil-lion-left "gL" #'evil-lion-right)
    (evil-lion-mode))

(leaf evil-matchit
    :doc "Smarter `%` motion"
    :config
    (global-evil-matchit-mode 1))

(leaf evil-nerd-commenter
    :doc "Toggle comment"
    :commands (evilnc-comment-or-uncomment-lines))

(leaf evil-string-inflection
    :doc "Add `g~` operator to cycle through string cases"
    :url "https://github.com/ninrod/evil-string-inflection")

(leaf vimish-fold
    :after evil
    :config
    (leaf evil-vimish-fold
        :custom ((evil-vimish-fold-mode-lighter . " ⮒")
                 (evil-vimish-fold-target-modes quote
                                                (prog-mode conf-mode text-mode)))
        :config
        (global-evil-vimish-fold-mode)))

(evil-ex-define-cmd "Bd" #'kill-this-buffer)
(evil-ex-define-cmd "BD" #'kill-this-buffer)
(evil-ex-define-cmd "hs" #'evil-window-split)

(evil-ex-define-cmd "ed"
                    (lambda nil
                        (interactive)
                        (evil-edit
                         (concat user-emacs-directory "init.org"))))
(evil-ex-define-cmd "s" #'toy/reload)

(evil-ex-define-cmd "o"
                    (lambda nil
                        (interactive)
                        (evil-edit
                         (concat org-directory "/journal.org"))))

(defun toy/evil-quit ()
    (interactive)
    ;; FIXME: consider `neotree` (which closes automatically)
    (cond ((one-window-p) (toy/evil-quit-all))
          (t (evil-quit))))

(defun toy/evil-save-and-quit()
    (interactive)
    (save-buffer)
    (toy/evil-quit))

(defun toy/evil-quit-all ()
    (interactive)
    (cond ((= 1 (length (funcall tab-bar-tabs-function))) (evil-quit-all))
          ;; last window, not last tab
          (t (tab-bar-close-tab))
          ))

;; [Evil] do not quit Emacs if we have remaning tab[s]
;; NOTE: this is not perfect, e.g., when we press `C-w q`
(evil-ex-define-cmd "q[uit]" 'toy/evil-quit)
(evil-ex-define-cmd "wq" 'toy/evil-save-and-quit)
(evil-ex-define-cmd "qa[ll]" 'toy/evil-quit-all)

(defun toy/smart-recenter ()
    "Recenter or scroll to just before EoF"
    ;; TODO: taken into account visual line
    (interactive)
    (let ((max-ln (line-number-at-pos (buffer-size)))
          (ln (line-number-at-pos (point)))
          (current-scroll (line-number-at-pos (window-start)))
          (h (window-body-height)))
        (let ((smart-max-scroll (max 0 (+ scroll-margin (- max-ln (- h 1)))))
              (scroll (max 0 (- ln (/ h 2)))))
            (scroll-down (- current-scroll (min smart-max-scroll scroll)))
            )))

(setq-default toy/sidebar-width 25)
(setq-default toy/bottom-bar-height 7)
(defvar toy/sidebar-imenu-buffer-name "@imenu")
(defvar toy/bottom-vterm-buffer-name "⊥ vterm")

(defun toy/imenu-get-nearest ()
    "Returns `nil' or `(name . marker)' pair of the nearest item on `imenu'"
    (interactive)

    ;; Thanks: https://emacs.stackexchange.com/questions/30673/next-prev-imenu-item-function
    ;; (imenu--make-index-alist)

    (let ((alist imenu--index-alist)
          (minoffset (point-max))
          base-point offset pair mark imstack result)
        (save-excursion
            (move-end-of-line 1)
            (setopt base-point (point)))

        ;; Element = ("name" . marker)
        ;;         | ("submenu" ("name" . marker) ... )
        (while (or alist imstack)
            (if alist
                    (progn
                        (setq pair (car-safe alist)
                              alist (cdr-safe alist))
                        (cond
                         ((atom pair)) ;; Skip anything not a cons.

                         ((imenu--subalist-p pair)
                          (setq imstack   (cons alist imstack)
                                alist     (cdr pair)))

                         ((number-or-marker-p (setq mark (cdr pair)))
                          ;; REMARK: Allow zero, search direction = -1 (up)
                          (when (>= (setq offset (* (- mark base-point) -1)) 0)
                              (when (< offset minoffset) ;; Find the closest item.
                                  (setq minoffset offset
                                        result pair))))))

                ;; pop
                (setq alist   (car imstack)
                      imstack (cdr imstack))))

        result))

;; FIXME: error
(defun toy/lsp-imenu-update-focus ()
    "Move the `*lsp-ui-imenu*' buffer's point to the current item."
    (interactive)
    (when (and (bound-and-true-p lsp-ui-mode) (bound-and-true-p lsp-enable-imenu))
        (let ((window (get-buffer-window toy/sidebar-imenu-buffer-name)))
            (when window

                ;; get the name of the current item
                (let ((pair (toy/imenu-get-nearest)))
                    (when pair
                        (let ((pattern (concat "┃ " (car pair) "$")))

                            ;; search in the imenu buffer
                            (with-selected-window window
                                (goto-char 0)
                                (re-search-forward pattern nil 'no-error)

                                (move-beginning-of-line 1)
                                (scroll-right 1000)

                                ;; -----------------
                                ;; (toy/smart-recenter)

                                (hl-line-mode 1)
                                (hl-line-highlight)))))))))

;; (defun toy/lsp-imenu-on-swtich-buffer ()
;;     (when (get-buffer toy/sidebar-imenu-buffer-name)
;;         (with-selected-window (get-buffer-window)
;;             (lsp-ui-imenu)
;;             (toy/lsp-imenu-update-focus))))

;; (add-hook 'post-command-hook #'toy/lsp-imenu-update-focus)
;; (add-hook 'window-selection-change-functions #'toy/lsp-imenu-update-focus)
;; (add-hook 'window-configuration-change-hook #'toy/lsp-imenu-update-focus)

;; TODO: relace with `eat'
(defun toy/bottom-vterm ()
    (interactive)
    (let ((last-name nil))
        (when (boundp 'vterm)
            (setq last-name vterm-buffer-name))
        (setq vterm-buffer-name toy/bottom-vterm-buffer-name)
        (let ((buf (vterm--internal (lambda (_buf)))))
            ;; restore `vterm-buffer-name7
            (when last-name (setq vter-buffer-name last-name))

            (display-buffer-in-side-window buf '((side . bottom)))
            (let ((win (get-buffer-window buf)))
                (select-window win)
                (let ((dh (- toy/bottom-bar-height (window-body-height))))
                    (enlarge-window dh))))))

(defun toy/info-url ()
    "Returns current info URL"
    (interactive)
    (let* (;; `(emacs) Case Conversion'
           (name (Info-copy-current-node-name))
           (space-offset (string-match " " name))
           ;; `(emacs)'
           (manual-name (substring name 1 (- space-offset 1)))
           ;; `Case Conversion'
           (page-name (string-replace " " "-" (substring name (+ space-offset 1)))))
        ;; `https://www.gnu.org/software/emacs/manual/html_node/elisp/Case-Conversion.html'
        (message (concat "https://www.gnu.org/software/emacs/manual/html_node/" manual-name "/" page-name ".html"))))

(defun toy/info-open-browser ()
    "Opens the current info with the default browser"
    (interactive)
    (browse-url (toy/info-url)))

(defun toy/last-message()
    "Retrieves the last echoed message from the `Messages' buffer"
    (save-excursion
        (set-buffer "*Messages*")
        (save-excursion
            (forward-line (- 1 num))
            (backward-char)
            (let ((end (point)))
                (forward-line 0)
                (buffer-substring-no-properties (point) end)))))

;; Overwrite `evil-cleanup-insert-state' with `combine-change-calls' added. It's for MUCH faster
;; multi-line insertion in large files even with `tree-sitter'.
(with-eval-after-load 'evil
    (defun evil-cleanup-insert-state ()
        "Called when Insert or Replace state is about to be exited.
Handles the repeat-count of the insertion command."
        (when evil-insert-count
            (dotimes (_ (1- evil-insert-count))
                (when evil-insert-lines
                    (evil-insert-newline-below)
                    (when evil-auto-indent
                        (indent-according-to-mode)))
                (evil-execute-repeat-info (cdr evil-insert-repeat-info))))
        (when evil-insert-vcount
            (let ((buffer-invisibility-spec
                   (if (listp buffer-invisibility-spec)
                           ;; make all lines hidden by hideshow temporarily visible
                           (cl-remove-if (lambda (x) (eq (or (car-safe x) x) 'hs))
                                         buffer-invisibility-spec)
                       buffer-invisibility-spec)))
                (cl-destructuring-bind (line col vcount) evil-insert-vcount
                    (let* ((beg 0) (end 0))
                        (save-excursion
                            (goto-char (point-min))
                            (forward-line line)
                            (setq beg (point)))
                        (save-excursion
                            (goto-char (point-min))
                            (forward-line (1+ (+ vcount line)))
                            (setq end (point)))
                        (combine-change-calls beg end
                            (save-excursion
                                (dotimes (v (1- vcount))
                                    (goto-char (point-min))
                                    (forward-line (+ line v))
                                    (when (or (not evil-insert-skip-empty-lines)
                                              (not (integerp col))
                                              (save-excursion
                                                  (evil-move-end-of-line)
                                                  (>= (current-column) col)))
                                        (if (integerp col)
                                                (move-to-column col t)
                                            (funcall col))
                                        (dotimes (_ (or evil-insert-count 1))
                                            (evil-execute-repeat-info (cdr evil-insert-repeat-info)))))))))))))

(leaf auto-package-update
    :custom ((auto-package-update-delete-old-versions . t)
             (auto-package-update-interval . 7))
    :config
    (auto-package-update-maybe))

;; TODO: linuix o
(leaf xclip
    :config (xclip-mode))

(leaf macrostep
    :doc "interactive macro expander"
    :config
    (define-key emacs-lisp-mode-map
                (kbd "C-c e")
                'macrostep-expand))

;; (leaf blamer
;;     :straight (blamer :type git :host github :repo "Artawower/blamer.el")
;;     :custom ((blamer-idle-time . 0.3)
;;              (blamer-min-offset . 70))
;;     :custom-face (blamer-face \`
;;                               ((t :foreground "#7a88cf" :background nil :height 140 :italic t))))

(leaf centaur-tabs
    :url "https://github.com/ema2159/centaur-tabs"
    :after projectile
    :custom ((centaur-tabs--buffer-show-groups)
             (centaur-tabs-cycle-scope quote tabs)
             (centaur-tabs-set-bar quote under)
             (x-underline-at-descent-line . t)
             (centaur-tabs-style . "bar")
             (centaur-tabs-height . 24)
             (centaur-tabs-set-modified-marker . t)
             (centaur-tabs-gray-out-icons quote buffer)
             (centaur-tabs-show-navigation-buttons)
             (centaur-tabs-set-icons . t))
    :custom (centaur-tabs-buffer-groups-function function toy/centaur-tabs-group)
    :init
    ;; (when (display-graphic-p)
    ;;     (customize-set-variable centaur-tabs-set-icons 'nerd-icons))
    :config
    (defun toy/centaur-tabs-group nil
        "Add `Sidebar' and `Bottom bar' groups / use `projectile' buffer gruups"
        (cond
         ((string-equal "@"
                        (substring
                         (buffer-name)
                         0 1))
          '("Sidebar"))
         ((string-equal "⊥"
                        (substring
                         (buffer-name)
                         0 1))
          '("Bottom bar"))
         ((or (string-equal "COMMIT-EDITMSG" (buffer-name))
              (and (> (length (buffer-name)) 5)
                   (string-equal "magit"
                                 (substring
                                  (buffer-name)
                                  0 5))))
          '("magit"))
         (t
          (centaur-tabs-projectile-buffer-groups))))

    (centaur-tabs-mode t)
    :defer-config (centaur-tabs-headline-match))

(leaf diff-hl
    :custom-face
    ;; (diff-hl-insert . '((t (:foreground "#87edb9" :background "#87edb9"))))
    (diff-hl-change . '((t (:foreground "#c0b18b" :background "#c0b18b"))))
    (diff-hl-delete . '((t (:foreground "#d75f5f" :background "#d75f5f"))))
    :init
    (global-diff-hl-mode)
    (defun toy/on-diff-hl ()
        (unless (display-graphic-p) (diff-hl-margin-mode))
        (diff-hl-flydiff-mode))
    :hook
    ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
     (magit-post-refresh-hook . diff-hl-magit-post-refresh)
     (diff-hl-mode-hook . toy/on-diff-hl)))

(leaf dirvish
    :doc "A modern file manager based on dired mode"
    :req "emacs-27.1"
    :url "https://github.com/alexluigit/dirvish"
    :emacs>= 27.1)

(leaf doom-modeline
    :url "https://github.com/seagle0128/doom-modeline"
    :after nerd-icons
    :custom ((doom-modeline-icon . t)
             (doom-modeline-major-mode-icon . t)
             (doom-modeline-modal . t)
             ;; (doom-modeline-height . 1)
             (doom-modeline-buffer-encoding)
             ;; Is it working?
             (doom-modeline-buffer-file-name-style quote truncate-upto-project))
    :config
    ;; remove `Git':
    (advice-add 'vc-git-mode-line-string :filter-return
                (lambda (arg)
                    (substring arg 4)))
    (doom-modeline-mode))

(leaf minions
    :doc "Hide minor mode names in the [+] tab (no need for `diminish'!)"
    :custom ((minions-mode-line-lighter . "[+]")
             (doom-modeline-minor-modes . t))
    :config
    (minions-mode 1))

(leaf eat)

(leaf hl-todo
    :doc "highlight TODO, FIXME, etc."
    :custom ((hl-todo-highlight-punctuation . ":")
             (hl-todo-keyword-faces \`
                                    (("TODO" warning bold)
                                     ("FIXME" error bold)
                                     ("WARNING" warning bold)
                                     ("HACK" font-lock-constant-face bold)
                                     ("REVIEW" font-lock-keyword-face bold)
                                     ("NOTE" success bold)
                                     ("WIP" font-lock-keyword-face bold)
                                     ("REMARK" success bold)
                                     ("DEPRECATED" font-lock-doc-face bold))))
    :config
    (global-hl-todo-mode 1))

;; (leaf indent-bars
;;     :ensure nil
;;     :straight (indent-bar :type git :host github :repo "jdtsmith/indent-bars")
;;     :custom
;;     ((indent-bars-treesit-support . t)
;;      (indent-bars-treesit-ignore-blank-lines-types  '("module"))
;;      ;; FIXME: Haskell indentation guides are not properly rendered
;;      ;; https://github.com/jdtsmith/indent-bars/wiki/indent%E2%80%90bars-config-Wiki#tree-sitter-config
;;      (indent-bars-treesit-scope
;;       . '((haskell function_definition class_definition for_statement if_statement with_statement while_statement))))
;; 
;;     ;; It breaks tree-sitter highlight
;;     ;; :hook (prog-mode-hook . indent-bars-mode)
;;     :config
;;     (defun toy/setup-indent-bars-2 ()
;;         (interactive)
;;         (setq-local indent-bars-spacing-override 2))
;;     (with-eval-after-load 'haskell-mode
;;         (add-hook 'haskell-mode-hook #'toy/setup-indent-bars-2))
;;     (with-eval-after-load 'tree-sitter
;;         (add-hook 'tree-sitter-after-on-hook #'indent-bars-mode)))

(leaf neotree
    :url "https://github.com/jaypei/emacs-neotree"
    :after evil
    :after projectile
    :commands (neotree-quick-look)
    :init
    (setopt neo-theme 'nerd-icons)
    :custom ((neo-window-position quote right)
             (neo-window-width . toy/sidebar-width)
             (neo-window-fixed-size)
             (neo-show-hidden-files . t))
    :config
    (setopt neo-buffer-name "@tree")
    (evil-define-key 'normal neotree-mode-map
        "gh" #'neotree-select-up-node
        "oo" #'neotree-enter
        (kbd "RET") #'neotree-enter
        "ov" #'neotree-enter-vertical-split
        "oh" #'neotree-enter-horizontal-split
        "cd" #'neotree-change-root
        "cu" #'neotree-select-up-node
        (kbd "C-c C-u") #'neotree-select-up-node
        "cc" #'neotree-copy-node
        "mc" #'neotree-create-node
        "md" #'neotree-delete-node
        "mr" #'neotree-rename-node
        "h" #'neotree-hidden-file-toggle
        "r" #'neotree-refresh
        "q" #'neotree-hide
        (kbd "TAB")
        'neotree-stretch-toggle)
    :defer-config
    (defun neo-path--shorten (path length)
        "Override `neotree' header string"
        (file-name-nondirectory
         (directory-file-name path)))
    (advice-add 'neotree-select-up-node :after
                (lambda (&rest _)
                    (evil-first-non-blank))))

(leaf nerd-icons
    :leaf-defer nil
    :config
    (leaf nerd-icons-completion
        :hook (marginalia-mode-hook . nerd-icons-completion-marginalia-setup)
        :config
        (nerd-icons-completion-mode))
    (leaf nerd-icons-dired
        :hook (dired-mode-hook . nerd-icons-dired-mode))
    (leaf magit-file-icons
        :after magit
        ;; :init
        :hook (magit-status-mode-hook . magit-file-icons-mode)
        :custom
        (magit-file-icons-enable-diff-file-section-icons . t)
        (magit-file-icons-enable-untracked-icons . t)
        (magit-file-icons-enable-diffstat-icons . t)))

(leaf olivetti
    :doc "Zen mode *per buffer* (not per frame and that is great!)"
    :url "https://github.com/rnkn/olivetti"
    :commands (olivetti-mode)
    :custom (olivetti-body-width . 120))

(leaf rainbow-delimiters
    :config
    (define-globalized-minor-mode toy/global-rainbow-delimiters-mode rainbow-delimiters-mode
        (lambda nil
            (rainbow-delimiters-mode 1)))
    (toy/global-rainbow-delimiters-mode 1))

(leaf rainbow-mode
    :doc "show color codes like this: #c0b18b"
    :config
    (define-globalized-minor-mode toy/global-rainbow-mode rainbow-mode
        (lambda nil
            (rainbow-mode 1)))
    (toy/global-rainbow-mode 1))

(leaf zoom-window
    :doc "Zoom in to a pane"
    :url "https://github.com/emacsorphanage/zoom-window"
    ;; mistake?
    :commands (darkroom-mode))

(leaf aggressive-indent
    :hook (emacs-lisp-mode-hook scheme-mode-hook))

(leaf cmake-mode)

(leaf dap-mode)

(leaf editorconfig
    :config
    (editorconfig-mode 1))

(leaf emmet-mode
    :hook
    ((html-mode-hook . emmet-mode)
     (web-mode-hook . emmet-mode)
     (css-mode-hook . emmet-mode)
     (typescript-tsx-mode-hook . emmet-mode)
     (vue-mode-hook . emmet-mode)))

(leaf envrc
  :config
  (envrc-global-mode))

(leaf flycheck)

(leaf magit
    :url "https://github.com/magit/magit"
    :commands (magit)
    :after evil
    :custom
    ((magit-log-section-commit-count . 15)
     (magit-refresh-status-buffer . nil)
     (dired-vc-rename-file . t))
    :config
    (defun magit-rev-format (format &optional rev args)
        "lighter magit revision format"
        (let ((str (magit-git-string "log" "-1" "--no-patch"
                                     (concat "--format=" format)
                                     args
                                     (if rev
                                             (concat rev "^{commit}")
                                         "HEAD")
                                     "--")))
            (unless (string-equal str "")
                str)))

    (evil-define-key 'normal 'magit-mode-map "zz" #'recenter-top-bottom "z-" #'evil-scroll-line-to-bottom "zb" #'evil-scroll-line-to-bottom
        (kbd "z RET")
        #'evil-scroll-line-to-top "zt" #'evil-scroll-line-to-top))

;; (leaf magit-todos
;;     :commands (magit-todos-list)
;;     :after magit)

(leaf forge
    :doc "Use GitHub on Emacs")

(leaf difftastic
    :config
    (with-eval-after-load
            'magit-diff
        (transient-append-suffix 'magit-diff '(-1 -1)
            [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
             ("S" "Difftastic show" difftastic-magit-show)]))

    (add-hook 'magit-blame-read-only-mode-hook
              (lambda ()
                  (keymap-set magit-blame-read-only-mode-map
                              "D" #'difftastic-magit-show)
                  (keymap-set magit-blame-read-only-mode-map
                              "S" #'difftastic-magit-show))))

(leaf lsp-mode
    :after evil
    :commands (lsp-mode lsp-deferred)
    :doc "`lsp-semantic-token-enable' is set to `nil' preferring `tree-sitter'"
    :custom ((lsp-completion-provider . :none) ;; use `cape'
             (lsp-completion-show-kind)
             (lsp-enable-snippet . nil)
             (lsp-keymap-prefix)
             (lsp-idle-delay . 0.5)
             (lsp-log-io)
             (lsp-trace)
             (lsp-print-performance)
             (lsp-eldoc-enable-hover)
             (lsp-signature-auto-activate)
             (lsp-signature-render-documentation)
             (lsp-enable-symbol-highlighting)
             (lsp-headerline-breadcrumb-enable)
             (lsp-modeline-diagnostics-scope . :workspace)
             ;; This is for `emacs-lsp-booster'.
             ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
             (lsp-use-plists . toy/use-plists)
             (lsp-semantic-tokens-enable))

    :hook ((lsp-mode-hook . lsp-enable-which-key-integration)
           (lsp-mode-hook . hs-minor-mode)
           (c-mode-hook . lsp-deferred)
           (cpp-mode-hook . lsp-deferred)
           (lsp-completion-mode . my/lsp-mode-setup-completion))

    :init
    ;; https://github.com/minad/corfu/wiki#advanced-example-configuration-with-orderless
    (defun my/orderless-dispatch-flex-first (_pattern index _total)
        (and (eq index 0) 'orderless-flex))

    (defun my/lsp-mode-setup-completion ()
        (interactive)
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
              '(orderless))
        ;; Optionally configure the first word as flex filtered.
        (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
        ;; Optionally configure the cape-capf-buster.
        (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

    :init
    (when toy/use-plists ;; `emacs-lsp-booster'
        (defun lsp-booster--advice-json-parse (old-fn &rest args)
            "Try to parse bytecode instead of json."
            (or
             (when (equal (following-char) ?#)
                 (let ((bytecode (read (current-buffer))))
                     (when (byte-code-function-p bytecode)
                         (funcall bytecode))))
             (apply old-fn args)))
        (advice-add (if (progn (require 'json)
                               (fboundp 'json-parse-buffer))
                            'json-parse-buffer
                        'json-read)
                    :around
                    #'lsp-booster--advice-json-parse)
        (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
            "Prepend emacs-lsp-booster command to lsp CMD."
            (let ((orig-result (funcall old-fn cmd test?)))
                (if (and (not test?)                             ;; for check lsp-server-present?
                         (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                         lsp-use-plists
                         (not (functionp 'json-rpc-connection))  ;; native json-rpc
                         (executable-find "emacs-lsp-booster"))
                        (progn
                            (message "Using emacs-lsp-booster for %s!" orig-result)
                            (cons "emacs-lsp-booster" orig-result))
                    orig-result)))
        (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

    :config
    ;; Broken: https://github.com/emacs-lsp/lsp-mode/issues/3577
    (delete 'lsp-terraform lsp-client-packages)
    (progn
        (defun toy/c-on-save nil
            (when (eq major-mode 'c-mode)
                (lsp-format-buffer)))

        (add-hook 'before-save-hook #'toy/c-on-save)
        (defun toy/cpp-on-save nil
            (when (eq major-mode 'c++-mode)
                (lsp-format-buffer)))

        (add-hook 'before-save-hook #'toy/cpp-on-save))

    :defer-config
    (define-key evil-normal-state-map " l" lsp-command-map)
    (evil-define-key 'normal lsp-mode-map
        "K" #'lsp-describe-thing-at-point))

(leaf lsp-ui
    :commands lsp-ui-mode
    :hook (lsp-mode-hook . lsp-ui-mode)
    :after evil
    :custom ((lsp-idle-delay . 0.5)
             (lsp-ui-sideline-delay . 0)
             (lsp-ui-doc-delay . 0)
             (lsp-ui-doc-enable)
             (lsp-ui-doc-position quote top)
             (lsp-ui-sideline-show-diagnostics . t)
             (lsp-ui-sideline-show-hover)
             (lsp-ui-sideline-show-code-actions)))

(leaf lsp-ui-imenu
    :ensure nil
    :custom ((lsp-imenu-sort-methods quote
                                     (position))
             (lsp-imenu-index-symbol-kinds quote
                                           (Class Method Proeprty Constructor Enum Interface Function Variable Constant String Number Boolean Array Object Key Struct Event Operator))
             (lsp-ui-imenu-buffer-name . toy/sidebar-imenu-buffer-name)
             (lsp-ui-imenu-window-width . toy/sidebar-width))
    :hook (lsp-ui-imenu-mode-hook . hl-line-mode)
    :custom-face (hl-line quote
                          ((t
                            (:background "#458588"))))
    :defer-config (evil-define-key 'normal lsp-ui-imenu-mode-map
                      (kbd "TAB")
                      #'lsp-ui-imenu--view
                      (kbd "RET")
                      #'lsp-ui-imenu--visit) (advice-add 'lsp-ui-imenu--visit :after
                      (lambda (&rest _)
                          (toy/force-center))))

(leaf prettier
    :doc "Aggressive source format on save.
Maybe use `dir-locals.el' or similars rather than to hooks:
https://github.com/jscheid/prettier.el?tab=readme-ov-file#enabling-per-file--per-directory"
    :custom (prettier-inline-errors-flag . t)
    :hook (typescript-mode-hook . prettier-mode)
    :hook (css-mode-hook . prettier-mode))

(leaf tree-sitter
    :doc "Incremental parsing system"
    :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(leaf tree-sitter-langs
    :after tree-sitter
    :config
    ;; TODO: need this check?
    (with-eval-after-load 'typescript-tsx-mode
        (tree-sitter-require 'tsx)
        (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))
    ;; (with-eval-after-load 'rust-mode
    ;;     )
    )

(leaf vue-mode
    :config
    (add-hook 'vue-mode-hook #'lsp-deferred))

(leaf adoc-mode
    :mode ("\\.adoc\\'" . adoc-mode)
    :config
    (defun toy/init-adoc-mode nil
        (interactive)
        (outline-minor-mode)
        (setq-local electric-indent-mode nil))

    (add-hook 'LaTeX-mode-hook
              (lambda nil
                  (electric-indent-local-mode -1)))
    :hook toy/init-adoc-mode)

(leaf dhall-mode
    :mode "\\.dhall\\'"
    :hook (dhall-mode-hook . lsp-deferred)
    :hook (dhall-mode-hook . lsp-ui-mode)
    :custom ((dhall-use-header-line)
             (dhall-format-arguments
              `("--ascii"))))

(leaf markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :hook (markdown-mode . orgtbl-mode)
    :after evil
    :custom
    ((markdown-command . "multimarkdown")
     (markdown-hide-markup . t)
     (markdown-fontify-code-blocks-natively . t))
    :config
    (evil-define-key 'normal markdown-mode-map "z1"
        (_fn
         (outline-hide-sublevels 1))
        "z2"
        (_fn
         (outline-hide-sublevels 2))
        "z3"
        (_fn
         (outline-hide-sublevels 3))
        "z4"
        (_fn
         (outline-hide-sublevels 4))
        "z5"
        (_fn
         (outline-hide-sublevels 5))
        "z6"
        (_fn
         (outline-hide-sublevels 6))
        "z9"
        (_fn
         (outline-hide-sublevels 9))
        "z0" #'evil-open-folds))

(leaf ron-mode
    :mode (("\\.ron\\'" . ron-mode))
    :hook (ron-mode-hook lambda nil
                         (setopt comment-start "// "
                                 comment-end "")))

(leaf yaml-mode)

;; (leaf folding-mode
;;     :ensure nil
;;     :straight (folding-mode :type git :host github :repo "jaalto/project-emacs--folding-mode"))

(leaf slime
    :if (file-exists-p "~/.roswell/helper.el")
    ;; :ensure slime-company
    :init (load "~/.roswell/helper.el")
    :custom (inferior-lisp-program . "sbcl")
    ;; :custom (inferior-lisp-program "ros -Q run")
    ;; :config (slime-setup '(slime-fancy slime-company))
    )

(leaf csharp-mode)
(leaf omnisharp
    ;; https://github.com/OmniSharp/omnisharp-emacs#:~:text=omnisharp-emacs%20is%20a%20port,that%20works%20in%20the%20background.
    )

(setq-default lisp-body-indent 4
              indent-tabs-mode nil
              tab-width 4)

(leaf fish-mode)

(leaf go-mode
    :config
    (add-hook 'go-mode-hook
              ;; FIXME: it would work even if it's not in `go-mode`
              (_fn (add-hook 'before-save-hook #'lsp-format-buffer t t)
                   (add-hook 'before-save-hook #'lsp-organize-imports t t)
                   (lsp-mode)
                   (lsp-ui-mode)
                   (flycheck-mode))))

(leaf haskell-mode
    :url "https://github.com/haskell/haskell-mode"
    :hook ((haskell-mode-hook . lsp-deferred)
           ;; (haskell-mode-hook . toggle-truncate-lines)
           (haskell-literate-mode-hook . lsp-deferred))
    :config
    ;; TOO SLOW
    (setopt lsp-lens-enable nil)
    (defun ormolu-format-buffer ()
        "Formats current buffer with `ormolu'.
Thanks: `https://www.masteringemacs.org/article/executing-shell-commands-emacs'"
        (interactive)
        (setq last-point (point))
        (shell-command-on-region
         (point-min) (point-max)
         (format "ormolu --stdin-input-file %s" (buffer-file-name))
         ;; output buffer, replace?, name of error buffer, show it
         (current-buffer) t
         "*Ormolu Error Buffer*" t)
        (goto-char last-point))

    (leaf consult-hoogle)

    (leaf lsp-haskell
        :after lsp-mode
        :url "https://github.com/emacs-lsp/lsp-haskell")

    (evil-define-key 'normal 'haskell-mode-map
        (kbd "C-c h") #'consult-hoogle
        (kbd "C-c f") #'ormolu-format-buffer)

    ;; The `o` / `O` fix works anyways:
    ;; https://emacs.stackexchange.com/a/35877
    (defun haskell-evil-open-above ()
        (interactive)
        ;; (evil-digit-argument-or-evil-beginning-of-line)
        (evil-beginning-of-line)
        (haskell-indentation-newline-and-indent)
        (evil-previous-line)
        (haskell-indentation-indent-line)
        (evil-append-line nil))

    (defun haskell-evil-open-below ()
        (interactive)
        (evil-append-line nil)
        (haskell-indentation-newline-and-indent))

    (evil-define-key 'normal haskell-mode-map
        "o" 'haskell-evil-open-below
        "O" 'haskell-evil-open-above)

    (progn
        ;; https://github.com/phoityne/hdx4emacs
        (require 'dap-mode)
        (require 'dap-utils)

        (dap-mode 1)
        (dap-ui-mode 1)
        (dap-tooltip-mode 1)
        (tooltip-mode 1)
        (setopt debug-on-error t)

        (dap-register-debug-provider
         "hda"
         ;; FIXME:
         (lambda (conf)
             (plist-put conf :dap-server-path (list "haskell-debug-adapter" "--hackage-version=0.0.31.0"))
             conf))

        (dap-register-debug-template
         "haskell-debug-adapter"
         (list :type "hda"
               :request "launch"
               :name "haskell-debug-adapter"
               :internalConsoleOptions "openOnSessionStart"
               :workspace (lsp-find-session-folder (lsp-session) (buffer-file-name))
               ;; :workspace "C:/work/haskell/sample"
               :startup "~/dev/hs/abc-hs/arc179/c/Main.hs"
               ;; :startup "C:/work/haskell/sample/app/Main.hs"
               :startupFunc ""
               :startupArgs ""
               :stopOnEntry t
               :mainArgs ""
               :ghciPrompt "H>>= "
               :ghciInitialPrompt "Prelude>"
               :ghciCmd "stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show"
               :ghciEnv (list :dummy "")
               ;; :logFile "C:/work/haskell/sample/hdx4emacs.log"
               :logFile "/tmp/my-dap-haskell"
               :logLevel "WARNING"
               :forceInspect nil)))

    )

(leaf idris-mode
    :after lsp-mode
    :mode "\\.l?idr\\'"
    :hook lsp-deferred
    :custom
    (idris-interpreter-path . "idris2")
    :config
    (add-to-list 'lsp-language-id-configuration '(idris-mode . "idris2"))

    ;; (with-eval-after-load 'lsp-mode
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "idris2-lsp")
      :major-modes '(idris-mode)
      :server-id 'idris2-lsp)))

;; (leaf koka-mode
;;     :ensure nil
;;     :load-path `,(concat user-emacs-directory "straight/repos/koka/support/emacs/")
;;     :straight (koka-mode :type git :host github :repo "koka-lang/koka")
;;     :require t)

(leaf lua-mode)

(leaf nix-mode
    ;; :mode "\\.nix\\'"
    :hook (nix-mode-hook . lsp-deferred)
    :custom (lsp-nix-nil-formatter . ["alejandra"]))

(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.ocaml\\'" . tuareg-mode))
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)

(leaf ocamlformat
    :init
    (defun reserve-ocaml-format-on-save ()
        (add-hook 'before-save-hook 'ocamlformat-before-save))
    :hook (tuareg-mode-hook . reserve-ocaml-format-on-save))

(leaf flycheck-ocaml
    :custom (merlin-error-after-save . nil)
    :hook ((tuareg-mode-hook . flycheck-ocaml-setup)
           (tuareg-mode-hook . flycheck-mode)))

(leaf flycheck-inline
    :hook (tuareg-mode-hook . flycheck-inline-mode))

(leaf dune)
(leaf utop
    :hook (tuareg-mode-hook . utop-minor-mode))

(leaf prolog-mode
    :ensure nil
    :tag "builtin"
    ;; It's Prolog, not Perl!
    :mode "\\.l?pl\\'"
    :hook toy/on-prolog)

(defun toy/on-prolog ()
    (lsp-mode)
    (lsp-ui-mode)

    (add-to-list 'lsp-language-id-configuration '(prolog-mode . "prolog")

                 (lsp-register-client
                  (make-lsp-client
                   :new-connection
                   (lsp-stdio-connection (list "swipl"
                                               "-g" "use_module(library(lsp_server))."
                                               "-g" "lsp_server:main"
                                               "-t" "halt"
                                               "--" "stdio"))

                   :major-modes '(prolog-mode)
                   :priority 1
                   :multi-root t
                   :server-id 'prolog-ls))))

(leaf glsl-mode
    :mode (("\\.fs" . glsl-mode)
           ("\\.vs" . glsl-mode)
           ("\\.glsl" . glsl-mode)
           ("\\.frag" . glsl-mode)
           ("\\.vert" . glsl-mode)))

(leaf gnuplot-mode
    (leaf gnuplot-mode
        :mode (("\\.gp\\'" . gnuplot-mode)))
    :mode (("\\.gp\\'" . gnuplot-mode)))

(leaf sql-indent
    :after sql)

(leaf sqlite-mode-extras
    :url "https://github.com/xenodium/sqlite-mode-extras"
    :hook ((sqlite-mode-hook . sqlite-extras-minor-mode)))

(leaf vimrc-mode
    :mode ("\\.vim" . vimrc-mode)
    :mode ("\\.nvim" . vimrc-mode))

(leaf rust-mode
    :hook (rust-mode-hook . lsp-deferred)
    :hook (rust-mode-hook . toy/on-rust-mode)
    :mode ("\\.rs\\'" . rust-mode)

    :custom ((rust-load-optional-libraries . t)
             (rust-format-on-save . t)
             (rust-format-show-buffer)
             (lsp-rust-analyzer-server-display-inlay-hints))

    :init
    ;; (defun rust-after-save-method ())
    (defun toy/on-rust-mode nil
        (interactive)
        (visual-line-mode)
        (setopt fill-column 100)
        (turn-on-auto-fill))

    ;; (defun toy/on-save-rust ()
    ;;     (lsp-format-buffer)
    ;;     (centaur-tabs-on-saving-buffer)
    ;; :config
    ;; (add-hook 'after-save-hook #'toy/on-save-rust)
    )

(leaf typescript-mode
    :mode "\\.ts\\'"
    :init
    (define-derived-mode typescript-tsx-mode typescript-mode "TSX"
        "Major mode for editing TSX files.")
    (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
    (add-to-list 'auto-mode-alist '("\\.eslintrc.cjs\\'" . typescript-ts-mode))
    :custom (typescript-indent-level . 2)
    :hook (typescript-mode-hook . lsp-deferred)
    :config
    ;; Using prettier instaead, but both should be unified
    ;; :config
    ;; (add-hook 'before-save-hook #'lsp-format-buffer)
    )

;; (leaf wgsl-mode
;;     :doc "cargo install --git https://github.com/wgsl-analyzer/wgsl-analyzer wgsl_analyzer"
;;     :ensure nil
;;     :straight (wgsl-mode :type git :host github :repo "KeenS/wgsl-mode.el")
;;     :hook (wgsl-mode-hook . lsp-deferred)
;;     :hook (wgsl-mode-hook . lsp-ui-mode)
;;     :config
;;     (with-eval-after-load 'lsp-mode
;;         (add-to-list 'lsp-language-id-configuration
;;                      '(wgsl-mode . "wgsl"))
;;         (lsp-register-client
;;          (make-lsp-client :new-connection
;;                           (lsp-stdio-connection "~/.cargo/bin/wgsl_analyzer")
;;                           :major-modes
;;                           '(wgsl-mode)
;;                           :server-id 'wgsl))))

(leaf consult
    ;; Required if we don't use default UI (like when using `vertico`)
    ;; :hook (completion-list-mode-hook . consult-preview-at-point-mode)

    :custom
    ((consult-preview-raw-size . 1024000)
     (consult-preview-key  . "C-l")
     (consult-narrow-key   . "<")
     (consult-async-min-input . 2)
     (register-preview-delay . 0)
     (register-preview-function . #'consult-register-format)
     (xref-show-xrefs-function . #'consult-xref)
     (xref-show-definitions-function . #'consult-xref))

    :init
    ;; adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    :config
    ;; use `fd`
    (when (executable-find "fd")
        (setopt consult-find-command "fd --color=never --full-path ARG OPTS"))

    ;; `which-key` alternative
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; detect project root with `projectile'
    (autoload 'projectile-project-root "projectile")
    (setopt consult-project-root-function (lambda (_) (projectile-project-root)))

    :defer-config
    ;; TODO: how can I use it like org-switchb
    ;; FIXME: I fail to defer `org-mode' loading with the org-mode source buffer
    ;; (autoload 'org-buffer-list "org")
    ;; (defvar org-buffer-source
    ;;     `(:name     "Org"
    ;;                 :narrow   ?o
    ;;                 :category buffer
    ;;                 :state    ,#'consult--buffer-state
    ;;                 :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
    ;; (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)

    (add-to-list 'consult-buffer-sources
                 (list :name     "Tabs"
                       :narrow   ?t
                       :category 'tab
                       :face     'font-lock-doc-face
                       :open     #'tab-bar-select-tab-by-name
                       :items    #'(lambda () (mapcar #'(lambda (tab) (cdr (assq 'name tab))) (tab-bar-tabs))))
                 'append))

(leaf consult-ghq)

(leaf consult-dir)

(leaf consult-flycheck)

(leaf consult-lsp
    :after (consult lsp)
    :config
    (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(leaf vertico
    :doc "Show minibuffer items in rows"
    :hook (after-init-hook . vertico-mode)
    :custom
    ((vertico-cycle . t)
     (vertico-count . 20)
     (vertico-scroll-margin . 4)))

(leaf orderless
    :doc "Find with space-separated components in any order"
    :leaf-defer nil
    :custom
    ((completion-styles . '(orderless partial-completion basic))
     (completion-category-defaults .  nil)
     ;; TODO: ?
     (completion-category-overrides . '((file (styles basic partial-completion))))
     ;; completion-category-overrides '((file (styles . (initials))))
     ))

(leaf emacs
    :init
    ;; add prompt indicator to `completing-read-multiple'.
    (defun crm-indicator (args)
        (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; grow/shrink minibuffer
    ;;(setopt resize-mini-windows t)

    ;; forbit the cursor in the minibuffer prompt
    (setopt minibuffer-prompt-properties
            '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    (setopt enable-recursive-minibuffers t))

;; Broken:

;; ;; Use `orderless':
;; ;; <https://github.com/minad/consult/wiki/Home/a0e391f8416e98b8d8319d62fb40b64f939b9fd1#use-orderless-as-pattern-compiler-for-consult-grepripgrepfind>
;; ;; better regex (copied from the README)
;; (defun consult--orderless-regexp-compiler (input type &rest _config)
;;     (setq input (orderless-compile input))
;;     (cons
;;      (mapcar (lambda (r) (consult--convert-regexp r type)) input)
;;      (lambda (str) (orderless--highlight input t str))))
;; 
;; ;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
;; (setopt consult--regexp-compiler #'consult--orderless-regexp-compiler)

;; OPTION 2: Activate only for some commands, e.g., consult-ripgrep!
;; (defun consult--with-orderless (&rest args)
;;     (minibuffer-with-setup-hook
;;             (lambda ()
;;                 (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
;;         (apply args)))
;; (advice-add #'consult-ripgrep :around #'consult--with-orderless)

(leaf marginalia
    :doc "Richer annotations in minibuffer"
    :hook (after-init-hook . marginalia-mode)
    :bind
    ;; press `C-q` to add/remove metadata
    (:minibuffer-local-map ("\C-q" . marginalia-cycle))

    :init
    (marginalia-mode)

    ;; automatically save the configuration
    (advice-add #'marginalia-cycle :after
                (lambda ()
                    (let ((inhibit-message t))
                        (customize-save-variable 'marginalia-annotator-registry
                                                 marginalia-annotator-registry))))

    ;; annotate `projectile-switch-project'
    (add-to-list 'marginalia-prompt-categories '("Switch to project" . file))

    ;; annotate `org-switchb'
    (add-to-list 'marginalia-prompt-categories '("Org buffer" . buffer))

    ;; FIXME: annotate `tab-bar-*-tab-by-name'
    (add-to-list 'marginalia-prompt-categories '("tab by name" . tab)))

(leaf embark
    :doc "Context menu in minibufffers"
    :url "https://github.com/oantolin/embark"
    :bind
    (:minibuffer-local-map
     (("C-c" . embark-act)
      ;; alternative for `describe-bindings'
      ("C-h B" . embark-bindings)))
    :bind
    (("C-." . embark-act) ;; not works on terminal
     ;; alternative for `describe-bindings'
     ("C-h B" . embark-bindings))

    :config
    (with-eval-after-load 'vertico
         (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
         (vertico-multiform-mode)
         (define-key vertico-map (kbd "\C-s") #'embark-act))

    :init
    (setopt prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

(leaf embark-consult
    :hook
    (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf corfu
    :doc "Be sure to configure `lsp-mode' with `corfu'"
    :url "https://github.com/minad/corfu"

    ;; Optional customizations
    :custom
    ((corfu-cycle .  t)
     (corfu-auto . t)                  ;; Enable auto completion
     (corfu-auto-delay . 0)
     (corfu-count . 10)
     (corfu-preselect . 'prompt)       ;; Do not select the first candidate
     (coruf-popupinfo-delay . 0.1)
     ;; (corfu-on-exact-match . nil)
     (tab-always-indent . 'complete)
     (completion-cycle-threshold .  3)
     (corfu-auto-prefix . 2))

    :config
    (global-corfu-mode)

    (with-eval-after-load 'orderless
        (add-hook 'corfu-mode-hook
                  (lambda ()
                      (setq-local orderless-matching-styles '(orderless-flex)))))

    ;; ;; https://github.com/minad/corfu#completing-in-the-minibuffer
    ;; (defun corfu-enable-in-minibuffer ()
    ;;     "Enable Corfu in the minibuffer."
    ;;     (when (local-variable-p 'completion-at-point-functions)
    ;;         ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    ;;         (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
    ;;                     corfu-popupinfo-delay nil)
    ;;         (corfu-mode 1)))
    ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
    )

;; (leaf kind-icon
;;     :url "https://github.com/jdtsmith/kind-icon"
;;     ;; :custom (kind-icon-default-face . 'corfu-default)
;;     :config
;;     ;; FIXME: void?
;;     ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
;;     (add-hook 'my-completion-ui-mode-hook
;;    	          (lambda ()
;;    	              (setopt completion-in-region-function
;;    		                  (kind-icon-enhance-completion
;;    		                   completion-in-region-function)))))

(leaf nerd-icons-corfu
    :after corfu
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(leaf cape
    :url "https://github.com/minad/cape"

    ;; Bind dedicated completion commands
    ;; Alternative prefix keys: C-c p, M-p, M-+, ...
    ;; :bind (("C-c p p" . completion-at-point) ;; capf
    ;;        ("C-c p t" . complete-tag)        ;; etags
    ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
    ;;        ("C-c p h" . cape-history)
    ;;        ("C-c p f" . cape-file)
    ;;        ("C-c p k" . cape-keyword)
    ;;        ("C-c p s" . cape-symbol)
    ;;        ("C-c p a" . cape-abbrev)
    ;;        ("C-c p i" . cape-ispell)
    ;;        ("C-c p l" . cape-line)
    ;;        ("C-c p w" . cape-dict)
    ;;        ("C-c p \\" . cape-tex)
    ;;        ("C-c p _" . cape-tex)
    ;;        ("C-c p ^" . cape-tex)
    ;;        ("C-c p &" . cape-sgml)
    ;;        ("C-c p r" . cape-rfc1345))

    :init
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    ;;(add-to-list 'completion-at-point-functions #'cape-history)
    ;; Programming language keyword
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-tex)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;; ELisp
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)
    )

;; (leaf popon
;;     :url "https://codeberg.org/akib/emacs-popon"
;;     :unless (display-graphic-p)
;;     :ensure nil
;;     :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon"))

;; (leaf corfu-terminal
;;     :url "https://codeberg.org/akib/emacs-corfu-terminal"
;;     :after corfu
;;     ;; :unless (display-graphic-p)
;;     :ensure nil
;;     :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal")
;;     :hook (corfu-mode-hook . corfu-terminal-mode)
;;     :config)

;; FIXME: too slow with lsp
;; (leaf corfu-popupinfo
;;     ;; Today it comes with `corfu'.
;;     :ensure nil
;;     :after corfu
;;     :custom ((coruf-popupinfo-delay . 0))
;;     :hook (corfu-mode-hook . corfu-popupinfo-mode))

(leaf tempel
    :doc "Tempo templates/snippets with in-buffer field editing"
    :url "https://github.com/minad/tempel"
    :config
    (setopt tempel-path (concat user-emacs-directory "tempel-snippets.el"))

    ;; FIXME: not working well?
    (defun tempel-setup-capf nil
        (setq-local completion-at-point-functions
                    (cons #'tempel-expand completion-at-point-functions)))
    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf)

    ;; TODO: Replace with `embark' (probably).
    ;; TODO: Use `corfu' and `cape' rather than direct call.
    (evil-define-key 'insert 'global
        (kbd "C-y") #'tempel-complete
        (kbd "C-l") #'tempel-insert
        (kbd "C-t") #'tempel-expand)

    ;; ;; FIXME: not working since it's not a mode?
    ;; (evil-define-key 'insert 'tempel-map
    ;;     "C-n" #'tempel-next
    ;;     "C-p" #'tempel-previous
    ;;     "C-<RET>" #'tempel-done)

    ;; ;; FIXME: not working since it's not a mode?
    ;; (evil-define-key 'normal 'tempel-map
    ;;     "C-<RET>" #'tempel-done)
    )

(defun toy/init-org ()
    (interactive)
    ;; Let's use logical lines. Line wrapping does not work well with Japanese text,
    ;; inserting needless whitespaces in output:
    (visual-line-mode)
    (setopt fill-column 100)
    ;; (turn-on-auto-fill)
    )

(leaf org
    :mode ("\\.org\\'" . org-mode)
    :hook
    ((org-mode-hook . toy/init-org)
     (org-babel-after-execute . org-redisplay-inline-images))
    :custom
    ((org-directory . "~/org")
     (org-cycle-emulate-tab . nil)
     (org-ditaa-jar-path . "~/.nix-profile/lib/ditaa.jar")
     (org-plantuml-jar-path . "~/.nix-profile/lib/plantuml.jar")
     ;; (ob-mermaid-cli-path .  "~/.nix-profile/bin/mmdc")
     ;; Do not count TODOs recursively
     (org-hierarchial-todo-statics . nil)
     ;; Do not indent source code
     (org-edit-src-content-indentation . 0)
     ;; Try `#+ATTR.*' keyword then fall back on the original width
     (org-image-actual-width . nil)
     (calendar-week-start-day . 1)
     (org-todo-keywords . '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")))
     (org-agenda-span . 7)
     (org-agenda-start-day . "+0d")
     (org-agenda-skip-timestamp-if-done . t)
     (org-agenda-skip-scheduled-if-done . t)
     (org-agenda-skip-scheduled-if-deadline-is-shown . t)
     (org-agenda-skip-timestamp-if-deadline-is-shown . t)
     (org-ellipsis . "⤵")
     (org-log-done . 'time)
     (org-src-fontify-natively . t)
     (org-use-speed-commands . t)
     (org-clock-persist . t)
     (org-time-clocksum-format . '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
     (org-tag-alist
      . '(("compe" . ?c)
          ("emacs" . ?e)
          ("life" . ?l)
          ("social" . ?s)
          ("work" . ?w)))

     ;; `https://emacs.stackexchange.com/a/17832'
     (org-agenda-prefix-format
      . '(
          ;; (agenda  . "  • ")
          (agenda  . " %i %t% s")
          ;; (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
          (timeline  . "  % s")
          (todo . " %i %-12:c")
          (tags  . " %i %-12:c")
          (search . " %i %-12:c"))))

    :config
    (setopt org-default-notes-file (concat org-directory "/tasks.org"))

    (setopt org-agenda-files
            (mapcar (lambda (path) (concat org-directory path))
                    '("/journal.org" "/timeline/")))

    ;; Start from Monday
    (progn ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
        ;; TODO: working?
        (font-lock-add-keywords 'org-mode
                                '(("^ *\\([-]\\) "
                                   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

        (custom-theme-set-faces
         'user
         '(org-block ((t (:inherit fixed-pitch))))
         '(org-code ((t (:inherit (shadow fixed-pitch)))))
         '(org-document-info ((t (:foreground "dark orange"))))
         '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
         '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
         '(org-link ((t (:underline t))))
         '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
         '(org-property-value ((t (:inherit fixed-pitch))) t)
         '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
         '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
         '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
         '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

    :defer-config
    (org-clock-persistence-insinuate))

(leaf ox-zenn
    :url "https://github.com/conao3/ox-zenn.el"
    :custom ((org-zenn-with-last-modified . nil)
             (org-export-with-toc . nil))
        :config
    (defun org-zenn-export-to-markdown-as (outfile &optional async subtreep visible-only)
        (interactive "sOutfile: ")
        (org-export-to-file 'zennmd outfile async subtreep visible-only))
        (defun org-babel-execute:shell (body params)
        "Execute a block of Shell commands with Babel.
        This function is called by `org-babel-execute-src-block'."
        (let* ((session (org-babel-sh-initiate-session
                         (cdr (assoc :session params))))
               (stdin (let ((stdin (cdr (assoc :stdin params))))
                          (when stdin (org-babel-sh-var-to-string
                                       (org-babel-ref-resolve stdin)))))
               (cmdline (cdr (assoc :cmdline params)))
               (full-body (org-babel-expand-body:generic
                           body params (org-babel-variable-assignments:shell params))))
            (org-babel-reassemble-table
             (org-babel-sh-evaluate session full-body params stdin cmdline)
             (org-babel-pick-name
              (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
             (org-babel-pick-name
              (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))
        (defun org-zenn-export-book-files (&optional org-dir)
        "`$org-dir' -> `$md-dir': `$zenn/books-org/$book' -> `$zenn/books/$book'"
        (interactive "sBook directory: ")
        (setopt org-dir (expand-file-name (or org-dir ".")))
        (let* ((book (file-name-nondirectory org-dir))
               (zenn (file-name-directory (directory-file-name (file-name-directory org-dir))))
               (md-dir (concat zenn "books/" book "/")))
            (if (not (and (file-directory-p org-dir) (file-directory-p md-dir)))
                    (message "Not an org book directroy?")
                (dolist (src-file-name (seq-filter (lambda (s) (string-suffix-p ".org" s)) (directory-files org-dir)))
                    (let* ((src-file (concat org-dir "/" src-file-name))
                           (dst-file-name (concat (file-name-base src-file-name) ".md"))
                           (dst-file (concat md-dir dst-file-name)))
                        (with-temp-buffer
                            (insert-file-contents src-file)
                            (org-zenn-export-to-markdown-as dst-file nil nil nil)))))))
        (defun org-zenn-export-buffer-to-book (&optional org-dir)
        "Runs subtree export to each level 1 headings. Respects `#+BOOK_DIR'."
        (interactive)
        (org-babel-tangle)
            ;; export all
        (let* ((default-dir default-directory)
               (pub-dir (car (cdr (car (org-collect-keywords '("BOOK_DIR")))))))
            ;; cd into the target directory
            (when pub-dir (cd pub-dir))
            ;; export all
            (unwind-protect
                    (org-map-entries
                     (lambda ()
                         (let* ((is-draft (org-entry-get nil "DRAFT"))
                                (exports (org-entry-get nil "EXPORT_FILE_NAME")))
                             (when (and (not is-draft) exports)
                                 (org-zenn-export-to-markdown nil t))))
                     "LEVEL=1")
                ;; be sure to come back to the default directory
                (when pub-dir (cd default-dir))))))

;; html view
(leaf org-preview-html
    :commands org-preview-html-mode org-preview-html/preview)

(leaf simple-httpd
    :doc "`httpd-serve-directory' mainly for the org site"
    :config
    (defun toy/org-serve ()
        (interactive)
        (httpd-serve-directory "out")))

(leaf org-appear
    :doc "Uninline format on cursor"
    :url "https://github.com/awth13/org-appear"
    :hook (org-mode-hook . org-appear-mode)
    :custom
    (org-appear-autolinks . t)
    (org-appear-inside-latex . t)
    (org-hide-emphasis-markers . t))

(leaf org-fragtog
  :doc "combo with `org-latex-preview'"
  :hook ((org-mode-hook . org-fragtog-mode)
         (org-mode-hook . org-latex-preview)))

(leaf org-superstar
    :commands org-superstar-mode
    :hook (org-mode-hook . org-superstar-mode)
    :custom
    (org-superstar-special-todo-items . nil))

;; C-c C-j: create entry
(leaf org-journal
    :custom ((org-journal-dir . "~/org/journal/")
             (org-journal-date-format . "%Y-%m-%d"))
    :config
    ;; FIXME: it's wrong
    ;; (setopt org-capture-templates
    ;;         '(("j" "Journal" entry (file+datetree "journal.org") "* ")))
    )

;; fold
(evil-define-key 'normal org-mode-map
    "{" #'evil-backward-paragraph
    "}" #'evil-forward-paragraph
    "za" #'org-cycle
    "zR" #'org-fold-show-all
    ;; close/open
    "zC" #'org-fold-hide-sublevels
    "zO" #'org-fold-show-subtree)

(evil-define-key 'visual org-mode-map
    "{" #'evil-backward-paragraph
    "}" #'evil-forward-paragraph)

(evil-define-key 'normal org-mode-map
    "z1" (_fn (outline-hide-sublevels 1))
    "z2" (_fn (outline-hide-sublevels 2))
    "z3" (_fn (outline-hide-sublevels 3))
    "z4" (_fn (outline-hide-sublevels 4))
    "z5" (_fn (outline-hide-sublevels 5))
    "z6" (_fn (outline-hide-sublevels 6))
    "z7" (_fn (outline-hide-sublevels 7))
    "z8" (_fn (outline-hide-sublevels 8))
    "z9" (_fn (outline-hide-sublevels 9))
    "z0" #'evil-open-folds)

(leaf ob-mermaid
    :config
    (with-eval-after-load 'org-src
        (add-to-list 'org-src-lang-modes '("mermaid" . mermaid))))

(with-eval-after-load 'org-babel
    (org-babel-do-load-languages
     'org-babel-load-languages '((ditaa . t)
                                 (mermaid . t)
                                 (plantuml . t)
                                 (dot . t)
                                 (haskell . t)
                                 (shell . t)
                                 (sqlite . t))))

;; requires `math-preview' executable.
;; (leaf math-preview)

(leaf org-roam
    :custom
    ;; any effect?
    ((org-roam-v2-ack . t)
     (org-roam-directory . "~/org-roam")
     ;; (org-refile-targets
     ;;   .   '(("~/org/gtd/gtd.org" :maxlevel . 3)
     ;;         ("~/org/gtd/someday.org" :level . 1)
     ;;         ("~/org/gtd/tickler.org" :maxlevel . 2)))
     )
    :config
    (org-roam-db-autosync-mode)
        ;; `https://github.com/syl20bnr/spacemacs/issues/14137#issuecomment-735437329'
    (defadvice org-roam-insert (around append-if-in-evil-normal-mode activate compile)
        "If in evil normal mode and cursor is on a whitespace character, then go into
        append mode first before inserting the link. This is to put the link after the
        space rather than before."
        (let ((is-in-evil-normal-mode (and (bound-and-true-p evil-mode)
                                           (not (bound-and-true-p evil-insert-state-minor-mode))
                                           (looking-at "[[:blank:]]"))))
            (if (not is-in-evil-normal-mode)
                    ad-do-it
                (evil-append 0)
                ad-do-it
                (evil-normal-state)))))

(leaf org-roam-ui
    :after org-roam
    :custom
    ((org-roam-ui-sync-theme . t)
     (org-roam-ui-follow . t)
     (org-roam-ui-update-on-save . t)
     (org-roam-ui-open-on-start . t)))

(leaf consult-org-roam
    :after org-roam
        :custom
    ;; Use `ripgrep' for searching with `consult-org-roam-search'
    (consult-org-roam-grep-func . #'consult-ripgrep)
    ;; Configure a custom narrow key for `consult-buffer'
    (consult-org-roam-buffer-narrow-key . ?r)
    ;; Display org-roam buffers right after non-org-roam buffers
    ;; in consult-buffer (and not down at the bottom)
    (consult-org-roam-buffer-after-buffers . t)
        :config
    (require 'consult-org-roam)
    ;; Activate the minor mode
    (consult-org-roam-mode 1)
        ;; Eventually suppress previewing for certain functions
    (consult-customize
     consult-org-roam-forward-links
     :preview-key (kbd "M-."))
        ;; :bind
    ;; ;; Define some convenient keybindings as an addition
    ;; ("C-c n e" . consult-org-roam-file-find)
    ;; ("C-c n b" . consult-org-roam-backlinks)
    ;; ("C-c n l" . consult-org-roam-forward-links)
    ;; ("C-c n r" . consult-org-roam-search)
    )

(evil-define-key 'normal org-mode-map
    " ol" #'org-roam-buffer-toggle
    " of" #'org-roam-node-find
    " og" #'org-roam-graph
    " oi" #'org-roam-node-insert
    " oc" #'org-roam-capture
    " oj" #'org-roam-dailies-capture-today
    " os" #'org-roam-db-sync
    " ota" #'org-roam-tag-add
    " otm" #'org-roam-tag-remove
    " oz" #'org-zenn-export-buffer-to-book)

(defun toy/open-calendar ()
    (interactive)
    (split-window-right)
    (other-window 1)
    (cfw:open-org-calendar))

(leaf calfw)
(leaf calfw-org
    :commands cfw:open-org-calendar)

;; `https://orgmode.org/manual/Breaking-Down-Tasks.html'
(defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-todo-log-states)   ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
    (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(leaf elfeed
    :url "https://github.com/skeeto/elfeed"
    ;; `y`: yank URL, `b`: browse in GUI browser
    ;; `r`: mark as read, `u`: mark as unread
    :config
    (setopt elfeed-feeds
            '(("https://matklad.github.io/feed.xml")
              ("https://cp-algorithms.com/feed_rss_created.xml"))))

(leaf org-sliced-images
  :hook (org-mode-hook . org-sliced-images-mode))

(leaf md4rd)

;; (leaf eww
;;     :commands eww eww-follow-link
;;     :init
;;     ;; (setopt browse-url-browser-function 'eww-browse-url)
;;     (setopt eww-search-prefix "http://www.google.com/search?q=")
;; 
;;     (defun eww-wiki (text)
;;         "Function used to search wikipedia for the given text."
;;         (interactive (list (read-string "Wiki for: ")))
;;         (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
;;                      (url-encode-url text))))
;; 
;;     :config
;;     ;; (add-hook 'eww-after-render-hook 'ha/eww-rerender-pages)
;;     ;; (add-hook 'eww-mode 'ace-link-mode)
;; 
;;     ;; :bind (("C-c w w" . eww)
;;     ;;        ("C-c w i" . eww-wiki)
;;     ;;        ("C-c w l" . eww-follow-link))
;;     )

(leaf hydra)

;; builtin!
(require 'winner)
(winner-mode 1)

(leaf windswap
    ;; https://github.com/amnn/windswap
    ;; windswap-left|right|up|down
    :commands (windswap-up windswap-down windswap-left windswap-right))

(defun toy/tab-move-right ()
    (interactive)
    (let* ((ix (tab-bar--current-tab-index))
           (n-tabs (length (funcall tab-bar-tabs-function)))
           (next-ix (mod (+ ix 1) n-tabs)))
        ;; use 1-based index
        (tab-bar-move-tab-to (+ 1 next-ix))))

(defun toy/tab-move-left ()
    (interactive)
    (let* ((ix (tab-bar--current-tab-index))
           (n-tabs (length (funcall tab-bar-tabs-function)))
           (next-ix (mod (+ ix n-tabs -1) n-tabs)))
        ;; use 1-based index
        (tab-bar-move-tab-to (+ 1 next-ix))))

(defvar toy/expand-unit 5)

(with-eval-after-load 'hydra
    (defhydra toy/hydra-window (:color red :hint nil)
        "
hjkl: focus     rR: rotate          t: tab (prefix)
HJKL: resize    =: equlize          C-h, C-l: tab focus
wasd: split     c/q: close          1-9: move to tab
WASD: swap      x: kill, X: both    u: undo window change
"

        ("u" winner-undo)
        ;; doesn't work
        ;; ("C-r" winner-redo)
    
        ;; switch to hydra for `tab-bar-mode` (Emacs 27)
        ;; ("t" (lambda () (interactive)
        ;;          (hydra--body-exit)
        ;;          (toy/hydra-tab/body)))
    
        ;; select centaur-tabs tabs
        ("1" (_fn (tab-bar-switch-to-tab 1)))
        ("2" (_fn (tab-bar-switch-to-tab 2)))
        ("3" (_fn (tab-bar-switch-to-tab 3)))
        ("4" (_fn (tab-bar-switch-to-tab 4)))
        ("5" (_fn (tab-bar-switch-to-tab 5)))
        ("6" (_fn (tab-bar-switch-to-tab 6)))
        ("7" (_fn (tab-bar-switch-to-tab 7)))
        ("8" (_fn (tab-bar-switch-to-tab 8)))
        ("9" (_fn (tab-bar-switch-to-tab 9)))
        ;; TODO: move tab
    
        ;; focus
        ("h" #'evil-window-left)
        ("j" #'evil-window-down)
        ("k" #'evil-window-up)
        ("l" #'evil-window-right)
    
        ("]"  #'evil-forward-section-begin)
        ("["  #'evil-backward-section-begin)
    
        ;; enlarge/shrink
        ("H" (_fn (enlarge-window-horizontally (- toy/expand-unit))))
        ("L" (_fn (enlarge-window-horizontally toy/expand-unit)))
        ("K" (_fn (enlarge-window (- toy/expand-unit))))
        ("J" (_fn (enlarge-window toy/expand-unit)))
    
        ;; split
        ("w" #'toy/sp-N)
        ("a" #'toy/sp-W)
        ("s" #'evil-window-split)
        ("d" #'evil-window-vsplit)
    
        ;; swap to neighbor
        ("W" (_fn (windswap-up)))
        ("A" (_fn (windswap-left)))
        ("S" (_fn (windswap-down)))
        ("D" (_fn (windswap-right)))
    
        ;; close
        ("c" #'toy/evil-quit) ;; NOTE: defined in `keymap.el`
        ("q" #'toy/evil-quit) ;; NOTE: defined in `keymap.el`
    
        ;; delete
        ("x" #'kill-this-buffer)
        ("X" #'evil-delete)
    
        ("C-s" #'save-buffer)
    
        ("r" #'evil-window-rotate-downwards)
        ("R" #'evil-window-rotate-upwards)
        ("=" #'balance-windows)
    
        ("z" #'toy/zen)
        ("b" 'evil-buffer-new)
    
        ("ESC" nil)))

(with-eval-after-load 'hydra
    (defhydra toy/hydra-tab (:color red :hint nil)
        "
hl      move        r   rename
HL      swap        n   new
[1-9]   focus       x   close
m[1-9]  move
"
    
        ("u" winner-undo)
        ;; doesn't work
        ;; ("C-r" winner-redo)
    
        ;; tab-bar-mode (Emacs 27)
        ;; `awesome-tab`: https://github.com/manateelazycat/awesome-tab
        ("h"  #'tab-bar-switch-to-prev-tab)
        ("l"  #'tab-bar-switch-to-next-tab)
        ("H"  #'toy/tab-move-left)
        ("L"  #'toy/tab-move-right)
        ;; TODO: swap tab
    
        ;; FIXME:
        ;; ("w" #'toy/hydra-window/body)
        ("w" (_fn (hydra-disable)
                  (toy/hydra-window/body)))
    
        ("rr" #'tab-bar-rename-tab)
        ;; rename to project name
        ("rp" #'toy/set-tab-name-default) ;; NOTE: defined in `ide.el`
    
        ("n" #'tab-bar-new-tab)
        ;; new tab and set name
        ("N" (_fn (tab-bar-new-tab)
                  (call-interactively 'tab-bar-rename-tab)))
        ("x" #'tab-bar-close-tab)
    
        ;; select tab
        ("1" (_fn (tab-bar-select-tab 1)))
        ("2" (_fn (tab-bar-select-tab 2)))
        ("3" (_fn (tab-bar-select-tab 3)))
        ("4" (_fn (tab-bar-select-tab 4)))
        ("5" (_fn (tab-bar-select-tab 5)))
        ("6" (_fn (tab-bar-select-tab 6)))
        ("7" (_fn (tab-bar-select-tab 7)))
        ("8" (_fn (tab-bar-select-tab 8)))
        ("9" (lambda () (interactive) (tab-bar-select-tab 9)))
    
        ;; move tab
        ("m1" (_fn (tab-bar-move-tab-to 1)))
        ("m2" (_fn (tab-bar-move-tab-to 2)))
        ("m3" (_fn (tab-bar-move-tab-to 3)))
        ("m4" (_fn (tab-bar-move-tab-to 4)))
        ("m5" (_fn (tab-bar-move-tab-to 5)))
        ("m6" (_fn (tab-bar-move-tab-to 6)))
        ("m7" (_fn (tab-bar-move-tab-to 7)))
        ("m8" (_fn (tab-bar-move-tab-to 8)))
        ("m9" (_fn (tab-bar-move-tab-to 9)))
        ("ESC" nil)))

(defvar toy/global-mode-map (make-sparse-keymap)
    "High precedence keymap.")

(define-minor-mode toy/global-mode
    "Global minor mode for higher precedence key mappings."
    :global t)

(toy/global-mode)

(dolist (state '(normal visual insert))
    (evil-make-intercept-map
     (evil-get-auxiliary-keymap toy/global-mode-map state t t)
     state))

(setopt evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil)

(evil-define-key 'normal help-mode-map
    "q" #'kill-this-buffer
    "Q" #'evil-delete-buffer)

;; Do not treat `_' as a word boundary (thought it still treats `-` as a word boundary):
(modify-syntax-entry ?_ "w")

;; Do not treat `-' as a word boundary on lisp mode
(modify-syntax-entry ?- "w" lisp-mode-syntax-table)
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)

;; Or, make motions based on _symbols_, instead of _words_:
;; (defalias 'forward-evil-word 'forward-evil-symbol)

;; c.f. https://evil.readthedocs.io/en/latest/faq.html

(defun toy/fix-point ()
    (unless (window-minibuffer-p)
        (when (= (point) (+ (point-max) 0))
            (forward-line -1))))
(add-hook 'post-command-hook (_fn (toy/fix-point)))

(with-eval-after-load 'evil
    (defadvice forward-evil-paragraph (around default-values activate)
        (let ((paragraph-start (default-value 'paragraph-start))
              (paragraph-separate (default-value 'paragraph-separate)))
            ad-do-it)))

(defun toy/do-tr-esc ()
    (or (evil-insert-state-p) (evil-normal-state-p)
        (evil-replace-state-p) (evil-visual-state-p)))

(defun toy/smart-esc-tr (_)
    (if (toy/do-tr-esc) (kbd "ESC") (kbd "C-g")))
(define-key key-translation-map (kbd "ESC") #'toy/smart-esc-tr)

(defun toy/vf--impl (target-char delta-move)
    (let* ((start-point (point))
           (start-col (current-column))
           (jump-point nil))
        (while (and (not jump-point)
                    (eq 0 (forward-line delta-move)))
            (when (and (eq (move-to-column start-col) start-col)
                       (eq (char-after) target-char))
                (setq jump-point (point))))
        (goto-char (or jump-point start-point))
        jump-point))

(defun toy/vf (&optional target-char)
    "Searches forward a character in the same column. Returns the point on jump or nil on failure."
    (interactive)
    (toy/vf--impl (or target-char (evil-read-key)) 1))

(defun toy/vF (&optional target-char)
    "Searches backward a character in the same column. Returns the point on jump or nil on failure."
    (interactive)
    (toy/vf--impl (or target-char (evil-read-key)) -1))

;; seems like we can't use keyboard macros for these mappings (?)

;; `x` -> `"_x`
(evil-define-operator toy/null-x (beg end type register)
    :motion evil-forward-char
    (interactive "<R><x>")
    (evil-delete beg end type ?_))
(define-key evil-normal-state-map "x" 'toy/null-x)

;; `s` -> `"_s` (use `d` to copy to the register)
(evil-define-operator toy/null-s (beg end type register)
    :motion evil-forward-char
    (interactive "<R><x>")
    (evil-change beg end type ?_))
(define-key evil-normal-state-map "s" 'toy/null-s)

;; more generic helper: https://github.com/syl20bnr/spacemacs/issues/6977#issuecomment-24^4014379

(advice-add 'evil-ex-search-next :after (lambda (&rest _) (recenter)))
(advice-add 'evil-ex-search-previous :after (lambda (&rest _) (recenter)))
;; and more.. (`]]` -> `]]z<RET>`, `[[` -> `[[z<RET>`
(advice-add 'evil-forward-section-begin :after #'evil-scroll-line-to-top)
(advice-add 'evil-backward-section-begin :after #'evil-scroll-line-to-top)

;; ` and '
(advice-add 'evil-goto-mark :after (lambda (&rest _) (recenter)))
(advice-add 'evil-goto-mark-line :after (lambda (&rest _) (recenter)))
;; `C-o` and `C-i`
(advice-add 'evil-jump-backward :after (lambda (&rest _) (recenter)))
(advice-add 'evil-jump-forward :after (lambda (&rest _) (recenter)))
;; `<number>G` FIXME: other than `G`
(advice-add 'evil-goto-line :after (lambda (&rest count) (recenter)))

(defun toy/backward-kill-line (arg)
    (interactive "p")
    (kill-line (- 1 arg)))

;; TODO: why can't use evil-define-key?
(define-key evil-ex-completion-map "\C-f" 'forward-char)
(define-key evil-ex-completion-map "\C-b" 'backward-char)
(define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
(define-key evil-ex-completion-map "\C-e" 'move-end-of-line)

(define-key evil-ex-completion-map "\C-d" 'evil-delete-char)
(define-key evil-ex-completion-map "\C-h" 'evil-delete-backward-char)
(define-key evil-ex-completion-map "\C-k" 'evil-delete-line)
(define-key evil-ex-completion-map "\C-u" 'toy/backward-kill-line)

(evil-define-key 'insert 'global
    "\C-a" #'evil-first-non-blank
    "\C-e" #'end-of-line
    "\C-f" #'evil-forward-char
    "\C-b" #'evil-backward-char
    "\C-d" #'evil-delete-char
    "\C-h" #'evil-delete-backward-char
    "\C-k" 'evil-delete-line
    "\C-u" #'toy/backward-kill-line)

;; (evil-define-key 'normal 'global
;;     "\C-_" (_fn (message "undo-tree-undo removed"))
;;     "\M-_" (_fn (message "undo-tree-redo removed")))

;; (evil-define-key 'insert 'global
;;     "\C-_" (_fn (message "undo-tree-undo removed"))
;;     "\M-_" (_fn (message "undo-tree-redo removed")))

;; https://gist.github.com/dotemacs/9a0433341e75e01461c9
(defun toy/parse-url (url)
    "convert a git remote location as a HTTP URL"
    (if (string-match "^http" url)
            url
        (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                  "https://\\2/\\3"
                                  url)))

(defun toy/magit-open-repo ()
    "open remote repo URL"
    (interactive)
    ;; FIXME: `magit-get' is not there
    (let ((url (magit-get "remote" "origin" "url")))
        (progn
            (browse-url (toy/parse-url url))
            (message "opening repo %s" url))))

;; TODO: replace with `embark'
;; TODO: store local/upstream for org mode etc.
(evil-define-key 'normal 'toy/global-mode-map
    ;; open link
    "gB" #'browse-url
    "gR" #'toy/magit-open-repo)

;; https://www.emacswiki.org/emacs/NeoTree
(defun toy/neo-proj ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
        (neotree-show)
        (if project-dir
                (if (neo-global--window-exists-p)
                        (progn
                            (neotree-dir project-dir)
                            (neotree-find file-name)))
            (message "Could not find git project root."))))

(defun toy/search-forward ()
    (interactive)
    (call-interactively #'evil-search-forward)
    (recenter))

(evil-define-key 'normal 'toy/global-mode-map
    " ;" #'shell-command
    " *" #'toy/search-forward

    " k" #'kill-this-buffer    ;; kill buffer
    " K" #'evil-delete-buffer  ;; kill buffer and :qclose the window

    " q" #'toy/evil-quit       ;; close window, tab or frame
    " Q" #'tab-bar-close-tab   ;; close tab
    ;; NOTE: more tab keys in `toy/hydra-window` (SPC w)

    ;; zen/zoom
    " zz" #'olivetti-mode
    " zx" #'zoom-window-zoom
    " zc" #'writeroom-mode
    " zd" #'darkroom-mode

    " w" #'toy/hydra-window/body
    " t" #'toy/hydra-tab/body
    ;; " o" #'toy/hydra-org/body
    )

(evil-define-key 'normal 'toy/global-mode-map
    ;; `projectile-*`
    " fc" #'projectile-invalidate-cache

    ;; `tab-bar-*` (Emacs 27)
    " ft" #'tab-bar-switch-to-tab

    ;; TODO: prefer fuzzy search
    " fR" #'consult-org-roam-file-find

    " fg" #'centaur-tabs-switch-group
    )

(evil-define-key 'normal 'toy/global-mode-map
    " e" (_fn (evil-vimish-fold-mode) (evil-vimish-fold-mode) (evil-close-folds)))

(define-key evil-read-key-map (kbd "C-j") (kbd "、"))
(define-key evil-read-key-map (kbd "C-l") (kbd "。"))

;; open magit in the full frame
(defun toy/magit-frame ()
    (interactive)
    (let ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
        (ignore magit-display-buffer-function)
        (magit)))

(defun toy/magit-tab ()
    (interactive)
    (tab-new)
    (toy/magit-frame))

(evil-define-key 'normal 'toy/global-mode-map
    "  r" #'exit-minibuffer ; reset Evil
    "  g" #'magit
    "  G" #'toy/magit-frame
    "  T" #'toy/magit-tab)

(evil-define-key 'normal 'toy/global-mode-map
    " ol" #' org-store-link
    ;; TODO: copy upstream link URL instead
    " oL" #' org-insert-link
    " oc" #' org-capture
    " oa" #' (lambda () (interactive) (org-agenda-list))

    " ob" #' org-iswitchb
    " or" #' org-refile
    ;; TODO: roam

    " oj" #' org-clock-goto
    " oo" #' org-clock-out

    ;; toggle
    " otl" #'org-toggle-link-display)

(defun toy/kill-all ()
    "Kill most buffers"
    (interactive)
    (dolist (buf (buffer-list))
        (let (name (buffer-name buf))
            (unless (or (string= name "*scratch*")
                        (string= name "*Messages*"))
                (kill-buffer buf)))))

(defun toy/reset ()
    "Kill buffers and go back to the dashboard."
    (interactive)
    (toy/kill-all)
    (delete-other-windows)

    ;; (dashboard-insert-startupify-lists)
    ;; (switch-to-buffer dashboard-buffer-name)

    (recenter))

(evil-define-key 'normal 'toy/global-mode-map
    "   x" #'toy/reset
    )

(defun toy/sidebar-imenu-focus ()
    (interactive)
    (when (string= (buffer-name) toy/sidebar-imenu-buffer-name)
        (windmove-left))
    (with-selected-window (get-buffer-window)
        (lsp-ui-imenu)))

(evil-define-key 'normal 'toy/global-mode-map
    ;; @Sidebar
    " ni" #'toy/sidebar-imenu-focus

    " nn" #'toy/neo-proj
    " nr" #'neotree-refresh
    " nt" #'neotree-toggle
    " nf" #'neotree-find
    " nq" #'neotree-quick-look

    ;; ⊥ Bottom pane (`vterm')
    " bv" #'toy/bottom-vterm
    )



(defun toy/tr-buf ()
    (interactive)
    (google-translate-buffer)
    (select-window (get-buffer-window "*Google Translate*"))
    (search-forward "[Listen]" nil nil 1)
    (read-only-mode -1)
    (delete-region 1 (line-end-position)))

;; t[r]anslate
(evil-define-key 'normal 'toy/global-mode-map
    " rb" #'toy/tr-buf
    " rk" #'google-translate-at-point
    )

(with-eval-after-load 'evil
    (evil-define-key 'normal outline-minor-mode-map
        "z1" (_fn (outline-hide-sublevels 3))
        "z2" (_fn (outline-hide-sublevels 4))
        "z3" (_fn (outline-hide-sublevels 5))
        "z4" (_fn (outline-hide-sublevels 6))
        "z5" (_fn (outline-hide-sublevels 7))
        "z6" (_fn (outline-hide-sublevels 8))
        "z9" (_fn (outline-hide-sublevels 11))
        "z0" #'evil-open-folds))

(evil-define-key 'insert 'global
    "\C-s" (_fn (evil-force-normal-state)
                (save-buffer)))

;; https://ddskk.readthedocs.io/

;; In Nix, I install `emacsPackages.ddskk' and `skk-dicts'.
;; (require 'ddskk)

;; TODO: git-gutter alternative (forgot the name)

;; Start `skk-mode' with `C-x j'
;; (require 'skk-autoloads)
;; (global-set-key "\C-x\C-j" 'skk-mode)
;; (global-set-key "\C-xj" 'skk-auto-fill-mode)
;; (global-set-key "\C-xt" 'skk-tutorial)

;; (setopt default-input-method "japanese-skk")

;; skk-kakutei key binding

(evil-define-key '(normal visual) 'global
    " /" 'evilnc-comment-or-uncomment-lines)

(defun toy/swap-line-up ()
    (let ((col (current-column)))
        (progn
            (forward-line)
            (transpose-lines -1)
            (move-to-column col)
            )))

(defun toy/swap-line-down ()
    (interactive)
    (let ((col (current-column)))
        (progn
            (forward-line)
            (transpose-lines 1)
            (forward-line -2)
            ;; we have to manually restore the column position if we modify the line
            (move-to-column col))))

(defun toy/insert-line-down (count)
    (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun toy/is-star (name)
    (or (string-prefix-p "*" name)
        (string-prefix-p "@" name)
        (string-prefix-p "⊥" name)))

(defun toy/skip-star (nav-fn)
    "Skip `*' buffer or `@' buffer"
    (if (toy/is-star (buffer-name))
            ;; stop
            (funcall nav-fn)
        (progn (funcall nav-fn)
               (while (toy/is-star (buffer-name)) (funcall nav-fn)))))

(defun toy/run-with-non-dedicated (nav-fn)
    "Run function turning of dedicated window"
    ;; **This is for the sidebar support!**
    (let ((win (get-buffer-window)))
        (cond ((window-dedicated-p win)
               (set-window-dedicated-p win nil)
               (funcall nav-fn)
               (set-window-dedicated-p win t))
              (t (funcall nav-fn)))))

(evil-define-key 'normal 'toy/global-mode-map
    ;; cycle through buffers
    "[b" (_fn (toy/run-with-non-dedicated #'centaur-tabs-backward))
    "]b" (_fn (toy/run-with-non-dedicated #'centaur-tabs-forward))

    ;; "[g" #'centaur-tabs-backward-group
    ;; "]g" #'centaur-tabs-forward-group
    "[g" (_fn (toy/skip-star #'centaur-tabs-backward-group))
    "]g" (_fn (toy/skip-star #'centaur-tabs-forward-group))

    "[{" #'centaur-tabs-move-current-tab-to-left
    "]}" #'centaur-tabs-move-current-tab-to-right

    "[t" #'tab-bar-switch-to-prev-tab
    "]t" #'tab-bar-switch-to-next-tab
    "[T" #'toy/tab-move-left   ;; NOTE: defined in `hydra.el`
    "]T" #'toy/tab-move-right  ;; NOTE: defined in `hydra.el`

    ;; goto previous/next hunk and center cursor
    "[c" (_fn (diff-hl-previous-hunk) (recenter))
    "]c" (_fn (diff-hl-next-hunk) (recenter))

    ;; go to next/previous error and center the cursor
    "[l" (_fn (previous-error) (recenter))
    "]l" (_fn (next-error) (recenter))

    ;; swap lines
    "[e" (_fn (toy/swap-line-up))
    "]e" (_fn (toy/swap-line-down))

    ;; insert newline keeping the cursor position
    "[ " (_fn  (save-excursion (evil-insert-newline-above)))
    "] " (_fn  (save-excursion (evil-insert-newline-below)))

    ;; cycle through windows
    "[w" #'evil-window-prev
    "]w" #'evil-window-next)

;; FIXME: just use let?
(defmacro _cd (&rest body)
    "Call consult function with `default-directory' as root directory."
    (declare (doc-string 1))
    `(lambda () (interactive)
         (let ((consult-project-root-functiom nil))
             ,@body
             )))

(evil-define-key 'normal 'toy/global-mode-map
    " :" #'execute-extended-command)

(evil-define-key 'visual 'toy/global-mode-map
    " :" #'execute-extended-command)

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

(leaf helpful
    :bind ([remap describe-command]
           . helpful-command) ([remap describe-key]
           . helpful-key)
    :after evil
    :init
    (evil-define-key 'normal helpful-mode-map "q" #'kill-this-buffer)
    (evil-define-key 'normal 'global "K" #'helpful-at-point))

(leaf hl-todo
    :doc "highlight TODO, FIXME, etc."
    :custom ((hl-todo-highlight-punctuation . ":")
             (hl-todo-keyword-faces \`
                                    (("TODO" warning bold)
                                     ("FIXME" error bold)
                                     ("WARNING" warning bold)
                                     ("HACK" font-lock-constant-face bold)
                                     ("REVIEW" font-lock-keyword-face bold)
                                     ("NOTE" success bold)
                                     ("WIP" font-lock-keyword-face bold)
                                     ("REMARK" success bold)
                                     ("DEPRECATED" font-lock-doc-face bold))))
    :config
    (global-hl-todo-mode 1))

(leaf git-link
    :commands (git-link git-link-commit)
    :commands (gl-line gl-today)
    :custom (git-link-use-commit . t)
    :config
    (defun gl-line nil
        (interactive)
        (let ((git-link-use-commit t))
            (ignore git-link-use-commit)
            (call-interactively #'git-link)))

    (defun gl-today nil
        (interactive)
        (let ((git-link-use-commit t)
              (git-link-open-in-browser t))
            (ignore git-link-use-commit git-link-open-in-browser)
            (call-interactively #'git-link))))

(leaf git-modes)

(leaf google-translate
    :doc "Emacs interface to Google Translate."
    :url "https://github.com/atykhonov/google-translate")

;; FIXME: not workings
;; TODO: hook mode
(leaf pdf-tools
    :init
    (defun toy/on-pdf-view ()
        ;; (leaf org-pdfview)
        (pdf-view-mode)
        (pdf-tools-enable-minor-modes)
        ;; TODO: run with timer
        (run-with-timer 1 nil #'pdf-view-fit-page-to-window)
        (pdf-outline-imenu-enable)
        )
    :config
    :hook  (doc-view-mode-hook . toy/on-pdf-view))

(leaf persistent-scratch
    :config
    (persistent-scratch-setup-default))

(leaf popup)

(leaf projectile
    :leaf-defer nil
    :custom (projectile-enable-caching . nil)
    :config
    (projectile-mode 1))

(leaf scratch-comment
    :bind ((lisp-interaction-mode-map
            :package elisp-mode
            ("C-j" . scratch-comment-eval-sexp))))

(leaf which-key
    :custom ((which-key-idle-delay . 0.01)
             (which-key-idle-secondary-delay . 0.01))
    :config
    (define-key help-map
                (kbd "M")
                'which-key-show-major-mode)
    (which-key-mode))

(leaf zig-mode
    :mode ("\\.zig\\'" . zig-mode)
    :custom ((lsp-zig-zls-executable . "/Users/tbm/zls/zls")
             (zig-format-on-save . t)
             (zig-format-show-buffer)))

(leaf llm)

(leaf llm
    :custom
    ((ellama-language . "Japanese")
     (ellama-naming-scheme . 'ellama-generate-name-by-llm))

    :config
    ;; TODO: leaf
    (require 'llm-ollama)
    (setopt ellama-provider (make-llm-ollama
                             :chat-model "codestral:22b-v0.1-q4_K_S"
                             :embedding-model "codestral:22b-v0.1-q4_K_S"))

    (setopt ellama-translation-provider (make-llm-ollama
                                         :chat-model "aya:35b-23-q4_K_S"
                                         :embedding-model "aya:35b-23-q4_K_S"))

    (setopt ellama-providers
            '(("codestral" . (make-llm-ollama
                              :chat-model "codestral:22b-v0.1-q4_K_S"
                              :embedding-model "codestral:22b-v0.1-q4_K_S"))
              ("gemma2" . (make-llm-ollama
                           :chat-model "gemma2:27b-instruct-q4_K_S"
                           :embedding-model "gemma2:27b-instruct-q4_K_S"))
              ("command-r" . (make-llm-ollama
                              :chat-model "command-r:35b"
                              :embedding-model "command-r:35b"))
              ("llama3.1" . (make-llm-ollama
                             :chat-model "llama3.1:8b"
                             :embedding-model "llama3.1:8b")))))

(leaf org-ai
    :hook (org-mode-hook . org-ai-mode)
    :custom (org-ai-default-chat-model . "gpt-4o")
    :init
    (org-ai-global-mode))

(defun toy/setup-theme ()
    (interactive)
    (leaf doom-themes
        ;; :custom
        ;; ((doom-themes-enable-bold . t)
        ;;  (doom-themes-enable-italic . t))
        )

    ;; FIXME: smyx not working with emacs overay?
    ;; (leaf smyx-theme
    ;;     :ensure nil
    ;;     :straight (smyx-theme :type git :host github :repo "tacit7/smyx"))
    (load-theme 'doom-opera t)
    ;; (load-theme 'smyx t)
    )

(defun toy/setup-light-theme ()
    (interactive)
    (leaf doom-themes
        ;; :custom
        ;; ((doom-themes-enable-bold . t)
        ;;  (doom-themes-enable-italic . t))

        :config
        ;; First load doom theme and then overwrite most colors
        (load-theme 'doom-opera-light t)))

(defun toy/on-start ()
(delete-other-windows)
(toy/setup-theme)
;; (toy/setup-light-theme)
)

(add-hook 'window-setup-hook #'toy/on-start)

;;; init.el ends here
