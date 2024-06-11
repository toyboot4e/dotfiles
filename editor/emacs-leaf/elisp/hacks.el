;; TODO: Do with slow escape on terminal + tmux:
;; https://evil.readthedocs.io/en/latest/faq.html#problems-with-the-escape-key-in-the-terminal

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

(with-eval-after-load 'neotree
    ;; `nerd-icons' support for `neotree'. Should be removed after merge.
    ;; https://github.com/jaypei/emacs-neotree/pull/359
    (defun neo--nerd-icons-icon-for-dir-with-chevron (dir &optional chevron padding)
        (let ((icon (nerd-icons-icon-for-dir dir))
              (chevron (if chevron (nerd-icons-octicon (format "nf-oct-chevron_%s" chevron) :height 0.8 :v-adjust -0.1) ""))
              (padding (or padding "\t")))
            (format "%s%s%s%s%s" padding chevron padding icon padding)))

    (defun neo-buffer--insert-fold-symbol (name &optional node-name)
        "Write icon by NAME, the icon style affected by neo-theme.
`open' write opened folder icon.
`close' write closed folder icon.
`leaf' write leaf icon.
Optional NODE-NAME is used for the `icons' theme"
        (let ((n-insert-image (lambda (n)
                                  (insert-image (neo-buffer--get-icon n))))
              (n-insert-symbol (lambda (n)
                                   (neo-buffer--insert-with-face
                                    n 'neo-expand-btn-face))))
            (cond
             ((and  (equal neo-theme 'classic))
              (or (and (equal name 'open)  (funcall n-insert-image "open"))
                  (and (equal name 'close) (funcall n-insert-image "close"))
                  (and (equal name 'leaf)  (funcall n-insert-image "leaf"))))
             ((equal neo-theme 'arrow)
              (or (and (equal name 'open)  (funcall n-insert-symbol "▾"))
                  (and (equal name 'close) (funcall n-insert-symbol "▸"))))
             ((equal neo-theme 'nerd)
              (or (and (equal name 'open)  (funcall n-insert-symbol "▾ "))
                  (and (equal name 'close) (funcall n-insert-symbol "▸ "))
                  (and (equal name 'leaf)  (funcall n-insert-symbol "  "))))
             ((and (display-graphic-p) (equal neo-theme 'icons))
              (unless (require 'all-the-icons nil 'noerror)
                  (error "Package `all-the-icons' isn't installed"))
              (setq-local tab-width 1)
              (or (and (equal name 'open)  (insert (all-the-icons-icon-for-dir-with-chevron (directory-file-name node-name) "down")))
                  (and (equal name 'close) (insert (all-the-icons-icon-for-dir-with-chevron (directory-file-name node-name) "right")))
                  (and (equal name 'leaf)  (insert (format "\t\t\t%s\t" (all-the-icons-icon-for-file node-name))))))
             ((equal neo-theme 'nerd-icons)
              (unless (require 'nerd-icons nil 'noerror)
                  (error "Package `nerd-icons' isn't installed"))
              (setq-local tab-width 1)
              (or (and (equal name 'open)  (insert (neo--nerd-icons-icon-for-dir-with-chevron (directory-file-name node-name) "down")))
                  (and (equal name 'close) (insert (neo--nerd-icons-icon-for-dir-with-chevron (directory-file-name node-name) "right")))
                  (and (equal name 'leaf)  (insert (format "\t\t\t%s\t" (nerd-icons-icon-for-file node-name))))))
             (t
              (or (and (equal name 'open)  (funcall n-insert-symbol "- "))
                  (and (equal name 'close) (funcall n-insert-symbol "+ "))))))))
