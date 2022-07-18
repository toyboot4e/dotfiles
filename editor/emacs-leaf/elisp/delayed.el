;; -*- lexical-binding: t -*-

;; --------------------------------------------------------------------------------
;; Presudo asynchronous execution

;; https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/c47f8eb7cd547b95ba91
(defvar toy/delayed-tasks nil)
(defvar toy/delayed-run-timer nil)

(defun toy/run-delayed-tasks ()
    ;; FIFO
    (setq toy/delayed-tasks (nreverse toy/delayed-tasks))
    (setq toy/delayed-run-timer
          (run-with-timer
           0.2 0.2
           (lambda ()
               (if toy/delayed-tasks
                       (eval (pop toy/delayed-tasks))
                   (cancel-timer toy/delayed-run-timer))))))

(defmacro with-delayed-execution (&rest body)
    (declare (indent 0))
    `(push ',(cons 'progn body) toy/delayed-tasks))

(defmacro load-delayed (&rest f)
    (declare (indent 0))
    `(push `(load-file ,f) toy/delayed-tasks))

;; --------------------------------------------------------------------------------
;; Delayed tasks

(add-hook 'after-inithook #'toy/setup-theme)

(defun toy/setup-theme ()
    (leaf doom-themes
        :config
        (setq doom-themes-enable-bold t
              doom-themes-enable-italic t)

        ;; First load doom theme and then overwrite most colors
        (load-theme 'doom-opera t)
        (leaf smyx-theme
            :straight (smyx-theme :type git :host github :repo "tacit7/smyx")
            :config
            (load-theme 'smyx t))))

;; (defun toy/load-yas ()
;;     (leaf yasnippet
;;         :diminish
;;         :custom (yas-prompt-functions '(yas-completing-prompt))
;;         :config
;;         ;; Enable `yasnippet' with delay
;;         (yas-global-mode)))

