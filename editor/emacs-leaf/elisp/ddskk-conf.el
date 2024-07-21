;; https://ddskk.readthedocs.io/

;; In Nix, I install `emacsPackages.ddskk' and `skk-dicts'.
(require 'ddskk)

;; TODO: git-gutter alternative (forgot the name)

;; Start `skk-mode' with `C-x j'
;; (require 'skk-autoloads)
;; (global-set-key "\C-x\C-j" 'skk-mode)
;; (global-set-key "\C-xj" 'skk-auto-fill-mode)
;; (global-set-key "\C-xt" 'skk-tutorial)

;; q    skk-toggle-kana         「かなモード」と「カナモード」間をトグル切り替えする
;; l    skk-latin-mode          「かなモード」又は「カナモード」から「アスキーモード」へ
;; L    skk-jisx0208-latin-mode 「かなモード」又は「カナモード」から「全英モード」へ
;; C-j  skk-kakutei             「アスキーモード」又は「全英モード」から「かなモード」へ

;; Q
;; 再変換

;; 変換箇所について

;; かな送りについて

;; 辞書登録について

;; 変換中: スペースで次へ、前へ行くには？？
;; 変換リスト: スペースで次へ、 x で前へ

;; かなモードにおいて
;; / から始めることでアルファベットを変換対象にできる (Greak/greek など)
;; \ から始めることで

(setq default-input-method "japanese-skk")

;; skk-kakutei key binding

