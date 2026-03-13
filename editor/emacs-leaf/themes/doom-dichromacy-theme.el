;;; doom-dichromacy-theme.el --- color theme suitable for color-blind users -*- lexical-binding: t; no-byte-compile: t; -*-

;; Originally from:
;; https://github.com/jwiegley/emacs-release/blob/master/etc/themes/dichromacy-theme.el

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-dichromacy-theme nil
  "Options for the `doom-dichromacy' theme."
  :group 'doom-themes)

(defcustom doom-dichromacy-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-dichromacy-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-dichromacy
    "A light, color-blind-friendly theme using the Wong 2011 palette."
  :background-mode 'light

  ;; name        default   256       16
  ((bg         '("#fefefe" "white"   "white"        ))
   (bg-alt     '("#f7f7f7" "#f7f7f7" "white"        ))
   (base0      '("#f7f7f7" "#f7f7f7" "white"        ))
   (base1      '("#efefef" "#efefef" "brightblack"  ))
   (base2      '("#e5e5e5" "#e5e5e5" "brightblack"  ))
   (base3      '("#d0d0d0" "#d0d0d0" "brightblack"  ))
   (base4      '("#848ea9" "#848ea9" "brightblack"  ))
   (base5      '("#555555" "#555555" "brightblack"  ))
   (base6      '("#333333" "#333333" "brightblack"  ))
   (base7      '("#1a1a1a" "#1a1a1a" "brightblack"  ))
   (base8      '("#000000" "black"   "black"        ))
   (fg         '("#000000" "black"   "black"        ))
   (fg-alt     '("#383a42" "#383a42" "brightblack"  ))

   (grey       '("#848ea9" "#848ea9" "brightblack"  ))
   (red        '("#d55e00" "#d55e00" "red"          ))
   (orange     '("#e69f00" "#e69f00" "brightred"    ))
   (green      '("#009e73" "#009e73" "green"        ))
   (teal       '("#007b5a" "#007b5a" "brightgreen"  ))
   (yellow     '("#f8ec59" "#f8ec59" "yellow"       ))
   (blue       '("#0072b2" "#0072b2" "brightblue"   ))
   (dark-blue  '("#005282" "#005282" "blue"         ))
   (magenta    '("#cc79a7" "#cc79a7" "magenta"      ))
   (violet     '("#b5608e" "#b5608e" "brightmagenta"))
   (cyan       '("#56b4e9" "#56b4e9" "brightcyan"   ))
   (dark-cyan  '("#2a8cbf" "#2a8cbf" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base3)
   (selection      base2)
   (builtin        blue)
   (comments       green)
   (doc-comments   (doom-darken green 0.15))
   (constants      red)
   (functions      red)
   (keywords       cyan)
   (methods        red)
   (operators      fg)
   (type           blue)
   (strings        grey)
   (variables      orange)
   (numbers        orange)
   (region         yellow)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-pad
    (when doom-dichromacy-padded-modeline
      (if (integerp doom-dichromacy-padded-modeline) doom-dichromacy-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base5)
   (modeline-bg     base2)
   (modeline-bg-inactive base3))

  ;;;; Base theme face overrides
  ((cursor                       :background (doom-lighten blue 0.3))
   (font-lock-comment-face       :foreground comments :slant 'italic)
   (font-lock-constant-face      :foreground constants :weight 'bold)
   (font-lock-keyword-face       :foreground keywords :weight 'bold)
   (font-lock-type-face          :foreground type :weight 'bold)
   (font-lock-variable-name-face :foreground variables :weight 'bold)
   (font-lock-builtin-face       :foreground builtin)
   ((line-number &override)              :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :foreground base8 :background base1)
   (hl-line :background base1 :extend t)
   (shadow  :foreground base4)
   (tooltip :background base1 :foreground fg)

   (isearch        :foreground fg :background magenta)
   (lazy-highlight :foreground fg :background (doom-blend magenta bg 0.4))
   (evil-ex-search :foreground fg :background (doom-blend blue bg 0.35) :weight 'bold)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))

   ;;;; centaur-tabs
   (centaur-tabs-selected             :background bg     :foreground fg    :box nil)
   (centaur-tabs-unselected           :background bg-alt :foreground base4 :box nil)
   (centaur-tabs-selected-modified    :background bg     :foreground teal  :box nil)
   (centaur-tabs-unselected-modified  :background bg-alt :foreground teal  :box nil)
   (centaur-tabs-active-bar-face      :background blue)
   (centaur-tabs-modified-marker-selected   :inherit 'centaur-tabs-selected   :foreground blue)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected :foreground blue)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background blue)
   ;;;; diff-mode <built-in>
   (diff-refine-added   :foreground (doom-darken green 0.2) :background (doom-blend green bg 0.3))
   (diff-refine-removed :foreground (doom-darken red   0.2) :background (doom-blend red   bg 0.3))
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))
   ;;;; helm
   (helm-candidate-number :background blue :foreground bg)
   ;;;; lsp-mode
   (lsp-ui-doc-background :background base0)
   ;;;; magit
   (magit-blame-heading               :foreground orange :background bg-alt)
   (magit-header-line                 :background (doom-blend blue bg 0.1) :foreground fg :weight 'bold)
   (magit-section-highlight           :background base1 :extend t)
   (magit-diff-file-heading-selection :foreground blue :weight 'bold :background (doom-blend blue bg 0.1) :extend t)
   (magit-diff-added-highlight        :foreground (doom-darken green 0.1)  :background (doom-blend green  bg 0.15) :weight 'bold :extend t)
   (magit-diff-base-highlight         :foreground (doom-darken orange 0.1) :background (doom-blend orange bg 0.15) :weight 'bold :extend t)
   (magit-diff-hunk-heading           :foreground base5 :background (doom-blend violet bg 0.3) :extend t)
   (magit-diff-hunk-heading-highlight :foreground fg :background (doom-blend violet bg 0.4) :weight 'bold :extend t)
   (magit-diff-removed                :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight      :foreground red :background (doom-blend red bg 0.15) :weight 'bold)
   ;;;; smerge-tool <built-in>
   (smerge-upper :background (doom-blend red   bg 0.1))
   (smerge-lower :background (doom-blend green bg 0.1))
   (smerge-base  :background (doom-blend blue  bg 0.1))
   ;;;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background base1)
   (mmm-default-submode-face       :background base1)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground cyan)
   ;;;; org <built-in>
   ((org-block &override)            :background base0)
   ((org-block-begin-line &override) :foreground fg :slant 'italic)
   ((org-date &override)             :foreground orange)
   (org-ellipsis :underline nil :background bg :foreground base4)
   ((org-quote &override)            :background base0)
   ;;;; posframe
   (ivy-posframe :background base0)
   ;;;; selectrum
   (selectrum-current-candidate :background base2)
   ;;;; vertico
   (vertico-current :background base2)
   ;;;; solaire-mode
   (solaire-hl-line-face :background base1 :extend t)
   (solaire-mode-line-face
    :inherit 'mode-line
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   ;;;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; whitespace <built-in>
   ((whitespace-tab         &override) :background (if (not (default-value 'indent-tabs-mode)) base0 'unspecified))
   ((whitespace-indentation &override) :background (if       (default-value 'indent-tabs-mode)  base0 'unspecified))

   ;;;; gnus
   (gnus-group-news-1     :weight 'bold :foreground red)
   (gnus-group-news-1-low :foreground red)
   (gnus-group-news-2     :weight 'bold :foreground orange)
   (gnus-group-news-2-low :foreground orange)
   (gnus-group-news-3     :weight 'bold :foreground cyan)
   (gnus-group-news-3-low :foreground cyan)
   (gnus-group-news-4     :weight 'bold :foreground magenta)
   (gnus-group-news-4-low :foreground magenta)
   (gnus-group-news-5     :weight 'bold :foreground blue)
   (gnus-group-news-5-low :foreground blue)
   (gnus-group-news-low   :foreground green)
   (gnus-group-mail-1     :weight 'bold :foreground red)
   (gnus-group-mail-1-low :foreground red)
   (gnus-group-mail-2     :weight 'bold :foreground orange)
   (gnus-group-mail-2-low :foreground orange)
   (gnus-group-mail-3     :weight 'bold :foreground cyan)
   (gnus-group-mail-3-low :foreground cyan)
   (gnus-group-mail-low   :foreground green)
   (gnus-header-content   :foreground magenta)
   (gnus-header-from      :weight 'bold :foreground blue)
   (gnus-header-subject   :foreground orange)
   (gnus-header-name      :foreground cyan)
   (gnus-header-newsgroups :foreground red)

   ;;;; message
   (message-header-name    :foreground cyan)
   (message-header-cc      :foreground red)
   (message-header-other   :foreground green)
   (message-header-subject :foreground orange)
   (message-header-to      :weight 'bold :foreground blue)
   (message-cited-text     :slant 'italic :foreground green)
   (message-separator      :weight 'bold :foreground magenta)

   ;;;; flyspell
   (flyspell-duplicate :weight 'unspecified :foreground 'unspecified
                       :slant 'unspecified :underline orange)
   (flyspell-incorrect :weight 'unspecified :foreground 'unspecified
                       :slant 'unspecified :underline magenta))

  ;;;; Base theme variable overrides
  ;; On a light theme fg=black, bg=white; remap so ANSI 0=black, 7=white
  ((ansi-color-names-vector
    (vconcat (mapcar #'doom-color '(fg red green yellow blue magenta cyan bg))))))

;;; doom-dichromacy-theme.el ends here
