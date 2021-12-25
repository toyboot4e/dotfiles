;; Browser, RSS and SNS

;; ------------------------------ Browsing ------------------------------

;; TODO: email manager

(leaf elfeed
      ;; https://github.com/skeeto/elfeed
      ;; `y`: yank URL, `b`: browse in GUI browser
      ;; `r`: mark as read, `u`: mark as unread
      )

(leaf eww
      :commands eww eww-follow-link
      :init
      ;; (setq browse-url-browser-function 'eww-browse-url)
      (setq eww-search-prefix "http://www.google.com/search?q=")

      (defun eww-wiki (text)
          "Function used to search wikipedia for the given text."
          (interactive (list (read-string "Wiki for: ")))
          (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
                       (url-encode-url text))))

      :config
      ;; (add-hook 'eww-after-render-hook 'ha/eww-rerender-pages)
      ;; (add-hook 'eww-mode 'ace-link-mode)

      ;; :bind (("C-c w w" . eww)
      ;;        ("C-c w i" . eww-wiki)
      ;;        ("C-c w l" . eww-follow-link))
      )

(leaf twittering-mode
      ;; https://www.emacswiki.org/emacs/TwitteringMode
      :after evil
      :commands twit
      :init
      (setq twittering-use-master-password t)
      (setq twittering-icon-mode (display-graphic-p))
      (setq twittering-convert-fix-size 24)
      ;; (setq twittering-number-of-tweets-on-retrieval 100)
      (setq twittering-max-number-of-tweets-on-retrieval 1000)
      ;; (twittering-enable-unread-status-notifier)
      ;; (setq twittering-display-remaining t)

      ;; TODO: in Evil?
      ;; 'f' and 'b' key can be used to change timeline.
      (setq twittering-initial-timeline-spec-string
            '(":home"
              ;; ":mentions"
              "toyboot4e" ; your twitter ID
              ;; ":favorites"
              ;; ":retweets_of_me"
              ;; ":retweeted_by_me"
              ;; "(:search/twittering-mode/+:search/twmode/)"
              ))

      ;; URI shorting
      (setq twittering-tinyurl-service 'goo.gl)

      :bind (:twittering-mode-map
             ("C-c f" . twittering-favorite))


      :defer-config
      (evil-define-key 'normal 'twittering-mode-map
          "gj" (_fn (twittering-next-status)
                    (evil-scroll-line-to-bottom (line-number-at-pos)))
          "gk" (_fn (twittering-prebious-status)
                    (evil-scroll-line-to-bottom (line-number-at-pos)))
          )
      )

(defvar toy/tw-icon-size 240)

;; show images (not only icons): https://github.com/hayamiz/twittering-mode/issues/136
(with-eval-after-load 'twittering-mode
    (defun *twittering-generate-format-table (status-sym prefix-sym)
        `(("%" . "%")
          ("}" . "}")
          ("#" . (cdr (assq 'id ,status-sym)))
          ("'" . (when (cdr (assq 'truncated ,status-sym))
                     "..."))
          ("c" .
           (let ((system-time-locale "C"))
               (format-time-string "%a %b %d %H:%M:%S %z %Y"
                                   (cdr (assq 'created-at ,status-sym)))))
          ("d" . (cdr (assq 'user-description ,status-sym)))
          ("f" .
           (twittering-make-string-with-source-property
            (cdr (assq 'source ,status-sym)) ,status-sym))
          ("i" .
           (when (and twittering-icon-mode window-system)
               (let ((url
                      (cond
                       ((and twittering-use-profile-image-api
                             (eq twittering-service-method 'twitter)
                             (or (null twittering-convert-fix-size)
                                 (member twittering-convert-fix-size '(48 73))))
                        (let ((user (cdr (assq 'user-screen-name ,status-sym)))
                              (size
                               (if (or (null twittering-convert-fix-size)
                                       (= 48 twittering-convert-fix-size))
                                       "normal"
                                   "bigger")))
                            (format "http://%s/%s/%s.xml?size=%s" twittering-api-host
                                    (twittering-api-path "users/profile_image") user size)))
                       (t
                        (cdr (assq 'user-profile-image-url ,status-sym))))))
                   (twittering-make-icon-string nil nil url))))
          ("I" .
           (let* ((entities (cdr (assq 'entity ,status-sym)))
                  text)
               (mapc (lambda (url-info)
                         (setq text (or (cdr (assq 'media-url url-info)) "")))
                     (cdr (assq 'media entities)))
               (if (string-equal "" text)
                       text
                   (let ((twittering-convert-fix-size toy/tw-icon-size))
                       (twittering-make-icon-string nil nil text)))))
          ("j" . (cdr (assq 'user-id ,status-sym)))
          ("L" .
           (let ((location (or (cdr (assq 'user-location ,status-sym)) "")))
               (unless (string= "" location)
                   (concat " [" location "]"))))
          ("l" . (cdr (assq 'user-location ,status-sym)))
          ("p" . (when (cdr (assq 'user-protected ,status-sym))
                     "[x]"))
          ("r" .
           (let ((reply-id (or (cdr (assq 'in-reply-to-status-id ,status-sym)) ""))
                 (reply-name (or (cdr (assq 'in-reply-to-screen-name ,status-sym))
                                 ""))
                 (recipient-screen-name
                  (cdr (assq 'recipient-screen-name ,status-sym))))
               (let* ((pair
                       (cond
                        (recipient-screen-name
                         (cons (format "sent to %s" recipient-screen-name)
                               (twittering-get-status-url recipient-screen-name)))
                        ((and (not (string= "" reply-id))
                              (not (string= "" reply-name)))
                         (cons (format "in reply to %s" reply-name)
                               (twittering-get-status-url reply-name reply-id)))
                        (t nil)))
                      (str (car pair))
                      (url (cdr pair))
                      (properties
                       (list 'mouse-face 'highlight 'face 'twittering-uri-face
                             'keymap twittering-mode-on-uri-map
                             'uri url
                             'front-sticky nil
                             'rear-nonsticky t)))
                   (when (and str url)
                       (concat " " (apply 'propertize str properties))))))
          ("R" .
           (let ((retweeted-by
                  (or (cdr (assq 'retweeting-user-screen-name ,status-sym)) "")))
               (unless (string= "" retweeted-by)
                   (concat " (retweeted by " retweeted-by ")"))))
          ("S" .
           (twittering-make-string-with-user-name-property
            (cdr (assq 'user-name ,status-sym)) ,status-sym))
          ("s" .
           (twittering-make-string-with-user-name-property
            (cdr (assq 'user-screen-name ,status-sym)) ,status-sym))
          ("U" .
           (twittering-make-fontified-tweet-unwound ,status-sym))
          ;; ("D" .
          ;;  (twittering-make-fontified-tweet-unwound ,status-sym))
          ("T" .
           ,(twittering-make-fontified-tweet-text
             `(twittering-make-fontified-tweet-text-with-entity ,status-sym)
             twittering-regexp-hash twittering-regexp-atmark))
          ("t" .
           ,(twittering-make-fontified-tweet-text
             `(twittering-make-fontified-tweet-text-with-entity ,status-sym)
             twittering-regexp-hash twittering-regexp-atmark))
          ("u" . (cdr (assq 'user-url ,status-sym)))))
    (advice-add #'twittering-generate-format-table :override #'*twittering-generate-format-table)
    (defface twitter-divider
        `((t (:underline (:color "grey"))))
        "The vertical divider between tweets."
        :group 'twittering-mode)
    (setq twittering-icon-mode t
          twittering-use-icon-storage t
          twittering-convert-fix-size 40
          twittering-status-format "
  %i  %FACE[font-lock-function-name-face]{  @%s}  %FACE[italic]{%@}  %FACE[error]{%FIELD-IF-NONZERO[❤ %d]{favorite_count}}  %FACE[warning]{%FIELD-IF-NONZERO[↺ %d]{retweet_count}}

%FOLD[   ]{%FILL{%t}
%QT{
%FOLD[   ]{%FACE[font-lock-function-name-face]{@%s}\t%FACE[shadow]{%@}
%FOLD[ ]{%FILL{%t}}
}}}

    %I

%FACE[twitter-divider]{                                                                                                                                                                                  }
"))
