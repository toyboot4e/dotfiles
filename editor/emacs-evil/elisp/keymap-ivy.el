;; Ivy keymap

(evil-define-key 'normal 'toy/global-mode-map
    " :" #'counsel-M-x
    )

(evil-define-key 'normal 'toy/global-mode-map
    ;; `counsel` tips
    ;; TIP: press `C-j` to enter the directory
    ;; TIP: press `C-l` to preview, `C-k` to kill

    ;; `counsel-*`
    " ff" #'counsel-find-file
    " fF" #'counsel-projectile-find-file
    " fb" #'counsel-switch-buffer
    " fB" #'counsel-switch-buffer-other-window
    " fp" #'counsel-projectile-switch-project
    " fr" #'counsel-recentf

    ;; `counsel-evil-*`
    " fm" #'counsel-evil-marks
    " fr" #'counsel-evil-registers
    )

;; [g]rep
(evil-define-key 'normal 'toy/global-mode-map
    " gb" #'swiper-isearch          ;; this buffer
    " gB" #'swiper-all              ;; all the buffers
    " gr" #'counsel-rg
    " gR" #'counsel-projectile-rg
    )

