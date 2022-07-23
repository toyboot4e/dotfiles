;; -*- lexical-binding: t -*-

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

(defun toy/on-start ()
    (delete-other-windows)
    (toy/setup-theme))

(add-hook 'window-setup-hook #'toy/on-start)
