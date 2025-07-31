;; -*- lexical-binding: t -*-

;; This file is loaded before the package system and GUI is initialized
;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html>

(setq package-enable-at-startup nil)

;; ;; Hide byte compile cache
;; ;; https://github.com/emacscollective/no-littering
;; (when (fboundp 'startup-redirect-eln-cache)
;;   (startup-redirect-eln-cache
;;    (convert-standard-filename
;;     (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(progn ;; Hide some builtin UI
  ;; GUI
  ;; FIXME: move it to init.org?
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tool-bar-mode -1))

  (menu-bar-mode -1)
  (blink-cursor-mode -1)

  (setq inhibit-startup-message t    ; don't show welcome screen
        ring-bell-function 'ignore   ; don't make beep sounds
        )

  ;; TUI (?)
  (setq visible-cursor nil))

(progn ;; Setup GC
  ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org

  ;; https://emacs-lsp.github.io/lsp-mode/page/performance
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq toy/gc 100000000) ; 100MB

  ;; maxmize GC on startup
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

  ;; and then reset it to the preferred value (`toy/gc`)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold toy/gc
                             gc-cons-percentage 0.1)))

  ;; Run GC every 60 seconds if emacs is idle.
  (run-with-idle-timer 60.0 t #'garbage-collect)

  ;; Prevent GC from happing in minibuffer
  (defun toy/gc-minibuf () (setq gc-cons-threshold most-positive-fixnum))
  (add-hook 'minibuffer-setup-hook #'toy/gc-minibuf)

  ;; Restore the preferred value after 1 second
  (defun toy/gc-restore () (run-at-time 1 nil (lambda () (setq gc-cons-threshold toy/gc))))
  (add-hook 'minibuffer-exit-hook #'toy/gc-restore))

(progn ;; Long lines
  (setq-default bidi-paragraph-direction 'left-to-right)
  (if (version<= "27.1" emacs-version)
      (setq bidi-inhibit-bpa t)))

;; Use `plists' for LSP deserialization. This is for `emasc-lsp-booster'.
;; - https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
;; - https://github.com/blahgeek/emacs-lsp-booster
;; See also: `lsp-use-lists' and `emacs-booster' in `flake.nix'.
(defvar toy/use-plists nil)
(when toy/use-plists
  (setenv "LSP_USE_PLISTS" "true"))

