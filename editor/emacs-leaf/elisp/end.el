;; -*- lexical-binding: t -*-

(defun toy/on-start ()
    (delete-other-windows)
    (toy/setup-theme))

(add-hook 'window-setup-hook #'toy/on-start)
