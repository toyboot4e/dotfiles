;; Hacks

;; If `d` delete one line, run `exit-minibuffer`:
;; https://github.com/emacs-evil/evil/issues/571
;; Or see:
;; https://emacs.stackexchange.com/questions/35946/strange-behaviour-on-evil-delete

;; ------------------------------ Global mode ------------------------------

;; Create high precedence key mappings mainly for `evil-collection`:
;; https://github.com/emacs-evil/evil-collection

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

;; ------------------------------ More to Evil ------------------------------

;; Be Evil everywhere: https://github.com/noctuid/evil-guide#use-evil-everywhere
(setq evil-emacs-state-modes nil
      evil-insert-state-modes nil
      evil-motion-state-modes nil)

(evil-define-key 'normal help-mode-map
    "q" #'kill-this-buffer
    "Q" #'evil-delete-buffer)

(progn ;; [Evil] Set up word policy
    ;; Do not treat `_` as a word boundary (thought it still treats `-` as a word boundary):
    (modify-syntax-entry ?_ "w")
    ;; Or, make motions based on _symbols_, instead of _words_:
    ;; (defalias 'forward-evil-word 'forward-evil-symbol)

    ;; c.f. https://evil.readthedocs.io/en/latest/faq.html
    )

(progn ;; [Evil] Prevent cursor from going to the next line of EoF
    (defun toy/fix-point ()
        (unless (window-minibuffer-p)
            ;; (when (= (point) (+ (point-max) 1))
            (when (= (point) (+ (point-max) 0))
                (forward-line -1))))
    (add-hook 'post-command-hook (_fn (toy/fix-point))))

;; [Evil] Let `{` and `}` skip multiple bullets (`* ..` in markdown) like Vim:
(with-eval-after-load 'evil
    (defadvice forward-evil-paragraph (around default-values activate)
        (let ((paragraph-start (default-value 'paragraph-start))
              (paragraph-separate (default-value 'paragraph-separate)))
            ad-do-it)))

(progn ;; [Evil] Translate `ESC` or `C-c` to `C-g`
    ;; it works in `counsel-M-x`, Ivy minibuffer and more

    ;; TODO: check if it's working
    ;; translate `C-c` and `ESC` to `C-g` when we're in Vim mode or in `company` menu
    (defun toy/do-tr-esc ()
        (or (evil-insert-state-p) (evil-normal-state-p)
            (evil-replace-state-p) (evil-visual-state-p)
            ))

    (defun toy/smart-esc-tr (prompt)
        (if (toy/do-tr-esc) (kbd "ESC") (kbd "C-g")))
    (define-key key-translation-map (kbd "ESC") #'toy/smart-esc-tr)

    (defun toy/smart-esc-tr (prompt)
        (if (toy/do-tr-esc) (kbd "C-c") (kbd "C-g")))
    (define-key key-translation-map (kbd "C-c") #'toy/smart-cc-tr)
    )

;; ------------------------------ Evil packages ------------------------------

;; [Evil] Smart escape with `jk` or `kj`
(use-package evil-escape
    :init (setq evil-escape-key-sequence "jk"
                evil-escape-unordered-key-sequence t)
    :config (evil-escape-mode))

;; [Evil] Smarter `%` motion
(use-package evil-matchit
    :config (global-evil-matchit-mode 1))

;; [Evil] Add `g~` operator to cycle through string cases: https://github.com/ninrod/evil-string-inflection
(use-package evil-string-inflection)

;; [Evil] Add `gl` and `gL` algin operators: https://github.com/edkolev/evil-lion
(use-package evil-lion
    :config
    (evil-define-key 'normal 'toy/global-mode-map
        "gl" #'evil-lion-left
        "gL" #'evil-lion-right)
    (evil-define-key 'visual 'toy/global-mode-map
        "gl" #'evil-lion-left
        "gL" #'evil-lion-right)
    (evil-lion-mode))

;; [Evil] `gx` to swap: https://github.com/Dewdrops/evil-exchange
(use-package evil-exchange
    :config
    (evil-exchange-install))

;; ------------------------------ Evil policies ------------------------------

(progn ;; [Evil] Use blackhole register for some keys
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
    )

(defun toy/force-center()
    (evil-scroll-line-to-center (line-number-at-pos)))

(progn ;; [Evil] Center cursor on search (`n` -> `nzz`, `N` -> `Nzz`)
    (advice-add 'evil-ex-search-next :after (lambda (&rest x) (toy/force-center)))
    (advice-add 'evil-ex-search-previous :after (lambda (&rest x) (toy/force-center)))
    ;; and more.. (`]]` -> `]]zz`, `[[` -> `[[zz`0
    (advice-add 'evil-forward-section-begin :after (lambda (&rest x) (toy/force-center)))
    (advice-add 'evil-backward-section-begin :after (lambda (&rest x) (toy/force-center)))
    )

(progn ;; [Evil] Center cursor on jump
    ;; ` and '
    (advice-add 'evil-goto-mark :after (lambda (&rest x) (toy/force-center)))
    (advice-add 'evil-goto-mark-line :after (lambda (&rest x) (toy/force-center)))
    ;; `C-o` and `C-i`
    (advice-add 'evil-jump-backward :after (lambda (&rest x) (toy/force-center)))
    (advice-add 'evil-jump-forward :after (lambda (&rest x) (toy/force-center)))
    ;; `<number>G`
    (advice-add 'evil-goto-line :after
                (lambda (&rest count)
                    (unless count (toy/force-center))))
    )

;; ------------------------------ Emacs-like ------------------------------

(defun toy/backward-kill-line (arg)
    (interactive "p")
    (kill-line (- 1 arg)))

(progn ;; Emacs-like in ex mode
    ;; TODO: why can't use evil-define-key
    (define-key evil-ex-completion-map "\C-f" 'forward-char)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-e" 'move-end-of-line)

    (define-key evil-ex-completion-map "\C-d" 'evil-delete-char)
    (define-key evil-ex-completion-map "\C-h" 'evil-delete-backward-char)

    (define-key evil-ex-completion-map "\C-k" 'evil-delete-line)
    (define-key evil-ex-completion-map "\C-u" 'toy/backward-kill-line))

;; Emacs-like in insert mode
(evil-define-key 'insert 'toy/global-mode-map
    "\C-a" #'evil-first-non-blank
    "\C-e" #'end-of-line
    "\C-f" #'evil-forward-char
    "\C-b" #'evil-backward-char

    "\C-d" #'evil-delete-char
    "\C-h" #'evil-delete-backward-char

    ;; NOTE: it overwrites digraph key
    "\C-k" 'evil-delete-line
    "\C-u" #'toy/backward-kill-line

    ;; NOTE: prefer to use `C-n` and `C-p` for completion
    ;; "\C-n" 'evil-next-line
    ;; "\C-p" 'evil-previous-line
    )

;; ------------------------------ Misc ------------------------------

(progn ;; `SPC /` to comment out
    (use-package evil-nerd-commenter
        :ensure t
        :commands (evilnc-comment-or-uncomment-lines))

    (evil-define-key '(normal visual) 'global
        " /" 'evilnc-comment-or-uncomment-lines)
    )

(progn ;; `C-s` for saving
    (evil-define-key 'insert 'global
        "\C-s" (_fn (evil-force-normal-state)
                    (save-buffer))
        )

    (with-eval-after-load 'company
        (define-key company-active-map "\C-s"
            (_fn (company-abort)
                 (evil-force-normal-state)
                 (save-buffer))))
    )

;; ------------------------------ [] ------------------------------

(progn ;; helpers
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
                (move-to-column col) ;; we have to manually restore the column position if we modify the line
                )))

    (defun toy/insert-line-down (count)
        (dotimes (_ count) (save-excursion (evil-insert-newline-below)))))

;; for smart buffer navigation
(defun toy/skip-star (nav-fn)
    (if (string-prefix-p "*" (buffer-name))
            (funcall nav-fn)
        (progn (funcall nav-fn)
               (while (string-prefix-p "*" (buffer-name)) (funcall nav-fn))
               )))

(evil-define-key 'normal 'toy/global-mode-map
    ;; cycle through buffers
    ;; "[b" (lambda () (interactive) (toy/skip-star #'evil-prev-buffer))
    ;; "]b" (lambda () (interactive) (toy/skip-star #'evil-next-buffer))
    "[b" #'centaur-tabs-backward
    "]b" #'centaur-tabs-forward

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
    "[c" (_fn (git-gutter:previous-hunk 1)
              (toy/force-center))
    "]c" (_fn (git-gutter:next-hunk 1)
              (toy/force-center))

    ;; go to next/previous error and center the cursor
    "[l" (_fn (previous-error)
              (toy/force-center))
    "]l" (_fn (next-error)
              (toy/force-center))

    ;; swap lines
    "[e" (_fn (toy/swap-line-up))
    "]e" (_fn (toy/swap-line-down))

    ;; insert newline keeping the cursor position
    "[ " (_fn  (save-excursion (evil-insert-newline-above)))
    "] " (_fn  (save-excursion (evil-insert-newline-below)))

    ;; cycle through windows
    "[w" #'evil-window-prev
    "]w" #'evil-window-next
    )

;; ------------------------------ Leaders ------------------------------

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

(progn ;; consider `tab-bar-mode` for quit commands

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
    )

(defun toy/search-forward ()
    (interactive)
    (call-interactively #'evil-search-forward)
    (toy/force-center))

(evil-define-key 'normal 'toy/global-mode-map
    " ;" #'shell-command
    " *" #'toy/search-forward

    " k" #'kill-this-buffer    ;; kill buffer
    " K" #'evil-delete-buffer  ;; kill buffer and :qclose the window

    " q" #'toy/evil-quit       ;; close window, tab or frame
    " Q" #'tab-bar-close-tab   ;; close tab
    ;; NOTE: more tab keys in `toy/hydra-window` (SPC w)

    ;; neotree
    " nn" #'toy/neo-proj
    " nr" #'neotree-refresh
    " nt" #'neotree-toggle
    " nf" #'neotree-find
    " nq" #'neotree-quick-look

    ;; zen/zoom
    " zz" #'olivetti-mode
    " zx" #'zoom-window-zoom
    " zc" #'writeroom-mode
    " zd" #'darkroom-mode

    " w" #'toy/hydra-window/body
    " t" #'toy/hydra-tab/body
    ;; " o" #'toy/hydra-org/body
    )

;; [f]ind
(evil-define-key 'normal 'toy/global-mode-map
    ;; `projectile-*`
    " fc" #'projectile-invalidate-cache

    ;; `tab-bar-*` (Emacs 27)
    " ft" #'tab-bar-switch-to-tab

    ;; TODO: prefer fuzzy search
    " fO" #'org-switchb

    " fg" #'centaur-tabs-switch-group
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

;; ;; [o]rg-mode
;; (evil-define-key 'normal 'toy/global-mode-map
;;     " ol" #' org-store-link
;;     " oc" #' org-capture
;;     " oa" #' org-agenda
;;     " ob" #' org-iswitchb
;;     " or" #' org-refile
;;     " oj" #' org-clock-goto
;;     " oo" #' org-clock-out
;;     ;; toggle
;;     " otl" #'org-toggle-link-display
;;     )

;; (evil-define-key 'normal 'org-mode-map
;;     ;; insert
;;     " oit" #' org-inlinetask-insert-task
;;     )

;; (evil-define-key 'normal 'org-mode-map
;;     ;; insert
;;     " oit" #' org-inlinetask-insert-task
;;     )

;; open magit in the full frame
(defun toy/magit-frame ()
    (interactive)
    (let ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
        (magit)))

(defun toy/magit-tab ()
    (interactive)
    (tab-new)
    (toy/magit-frame))

(evil-define-key 'normal 'toy/global-mode-map
    "  g" #'magit
    "  G" #'toy/magit-frame
    "  T" #'toy/magit-tab
    )

;; ------------------------------ Dashboard ------------------------------

(defun toy/kill-all ()
    "Kill most buffers"
    (interactive)
    (dolist (buf (buffer-list))
        (let (name (buffer-name buf))
            (unless (or (string= name "*scratch*")
                        (string= name "*Messages*")
                        )
                (kill-buffer buf)
                ))
        ))

(defun toy/reset ()
    "Kill buffers and go back to the dashboard."
    (interactive)
    (toy/kill-all)
    (delete-other-windows)

    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)

    (toy/force-center))

(evil-define-key 'normal 'toy/global-mode-map
    "   x" #'toy/reset
    )

;; ------------------------------ Japanese ------------------------------

;; 句読点ジャンプ
(define-key evil-read-key-map (kbd "C-j") (kbd "、"))
(define-key evil-read-key-map (kbd "C-l") (kbd "。"))

