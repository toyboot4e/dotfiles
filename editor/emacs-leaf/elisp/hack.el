;; TODO: Do with slow escape on terminal + tmux:
;; https://evil.readthedocs.io/en/latest/faq.html#problems-with-the-escape-key-in-the-terminal

;; Overwrite `evil-cleanup-insert-state' with `combine-change-calls' added. It's for MUCH faster
;; multi-line insertion in large files even with `tree-sitter'.
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
                                        (evil-execute-repeat-info (cdr evil-insert-repeat-info))))))))))))
