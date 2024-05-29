;; https://ddskk.readthedocs.io/

;; In Nix, I install `emacsPackages.ddskk' and `skk-dicts'.
(require 'ddskk)

;; Start `skk-mode' with `C-x j'
;; (require 'skk-autoloads)
;; (global-set-key "\C-x\C-j" 'skk-mode)
;; (global-set-key "\C-xj" 'skk-auto-fill-mode)
;; (global-set-key "\C-xt" 'skk-tutorial)

;; q    skk-toggle-kana         「かなモード」と「カナモード」間をトグル切り替えする
;; l    skk-latin-mode          「かなモード」又は「カナモード」から「アスキーモード」へ
;; L    skk-jisx0208-latin-mode 「かなモード」又は「カナモード」から「全英モード」へ
;; C-j  skk-kakutei             「アスキーモード」又は「全英モード」から「かなモード」へ

(setq default-input-method "japanese-skk")

;; skk-kakutei key binding

