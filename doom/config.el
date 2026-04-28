(require 'f)
(require 's)
(require 'dash)

(setq
 user-full-name "Ben O."
 user-mail-address "benny.ouattara@gmail.com")

(setq
 org-agenda-skip-scheduled-if-done t
 org-super-agenda-groups '((:name "Today"
                            :time-grid t
                            :scheduled today)
                           (:name "Due today"
                            :deadline today)
                           (:name "Important"
                            :priority "A")
                           (:name "Overdue"
                            :deadline past)
                           (:name "Due soon"
                            :deadline future)
                           (:name "Big Outcomes"
                            :tag "bo")))

(use-package! doom-nano-modeline
  :config
  (doom-nano-modeline-mode 1)
  (global-hide-mode-line-mode 1))

(pcase (system-name)
  ("onyx" (setq
           doom-font (font-spec :family "Iosevka" :size 19 :weight 'normal)
           doom-big-font (font-spec :family "Iosevka" :size 27)
           doom-theme 'modus-operandi))
  ("oryx" (setq
           doom-font (font-spec :family "Iosevka" :size 17 :weight 'normal)
           doom-big-font (font-spec :family "Iosevka" :size 25)
           doom-theme 'modus-operandi))
  ("kite" (setq
           doom-font (font-spec :family "Iosevka" :size 17 :weight 'normal)
           doom-big-font (font-spec :family "Iosevka" :size 25)
           doom-theme 'modus-operandi))
  ("otter" (setq
            doom-font (font-spec :family "Iosevka" :size 21 :weight 'normal)
            doom-big-font (font-spec :family "Iosevka" :size 29)
            doom-theme 'modus-operandi)))

(setq
 mac-command-modifier 'meta
 ns-command-modifier 'meta
 confirm-kill-emacs nil
 evil-insert-state-cursor 'bar
 evil-move-cursor-back nil
 display-line-numbers-type nil
 fancy-splash-image (expand-file-name "splash.png" doom-private-dir))

(when (string= "kite" (system-name))
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist
               '(ns-appearance . dark))

  (defun beno--auto-theme (appearance)
    (pcase appearance
      ('light (consult-theme 'modus-operandi))
      ('dark (consult-theme 'catppuccin))))

  (add-hook 'ns-system-appearance-change-functions #'beno--auto-theme))

(unless (display-graphic-p) 
  (require 'evil-terminal-cursor-changer)
  (etcc-on)

  (setq
   select-enable-clipboard t
   evil-motion-state-cursor 'box
   evil-visual-state-cursor 'box
   evil-normal-state-cursor 'box
   evil-insert-state-cursor 'bar
   evil-emacs-state-cursor  'hbar)
  
  (menu-bar-mode -1)
  (xterm-mouse-mode 1)
  (consult-theme 'catppuccin))

(setq
 org-startup-folded 'show2levels
 org-auto-align-tags nil
 sync-dir "~/Sync/"
 org-directory (concat sync-dir "org")
 org-agenda-files (ignore-errors (directory-files org-directory t "\\.org$" t))
 org-hide-emphasis-markers t
 org-tags-column -80
 org-refile-targets (quote ((nil :maxlevel . 3)))
 +org-capture-todo-file "tasks.org")

(after! org-journal
  (setq org-journal-enable-agenda-integration t)
  (map! :leader :desc "Open current journal" "j" #'org-journal-open-current-journal-file))

;; (when (not (file-exists-p (concat doom-cache-dir "tramp-histfile")))
;;   (make-empty-file (concat doom-cache-dir "tramp-histfile")))

;; (after! tramp
;;   (setq
;;    tramp-histfile-override "/dev/null"
;;    tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath=~/.ssh/master-%%r@%%h:%%p -o ControlPersist=30m")
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(map! :leader
      :desc "split with eshell"     ">" #'beno--eshell-toggle-right
      :desc "fuzzy search visible"  "a" #'evil-avy-goto-char-2
      :desc "line in visible"       "A" #'avy-goto-line
      :desc "open file other win"   "V" #'projectile-find-file-other-window
      :desc "open buffer other win" "v" #'switch-to-buffer-other-window
      :desc "calendar"              "o c" #'cfw:open-calendar-buffer)

(map! "C-s" #'consult-line)

(defun beno-evil-scroll-down ()
  (interactive)
  (evil-scroll-down evil-scroll-count)
  (evil-scroll-line-to-center nil))

(defun beno-evil-scroll-up ()
  (interactive)
  (evil-scroll-up evil-scroll-count)
  (evil-scroll-line-to-center nil))

(map! :n "C-d" #'beno-evil-scroll-down
      :n "C-u" #'beno-evil-scroll-up)

(when (equal "ignored" (system-name))
  (condition-case err
      (let* ((refs (cdr (doom-call-process "nix-store" "--query" "--referrers"
                                           (file-truename (executable-find "mu")))))
             (root (car (-filter (lambda (s) (s-contains? "emacs-mu4e" s))
                                 (s-split "\n" refs))))
             (version (s-chop-prefix "-" (cadr (s-split "emacs" root))))
             (path (concat root "/share/emacs/site-lisp/elpa/" version)))
        (add-to-list 'load-path path))
    (error (warn "Failed to locate mu4e: %s" err))))

(when (equal "ignored" (system-name))
  (after! mu4e
    (setq mu4e-update-interval 180))
  (setq +mu4e-workspace-name "*mail*")

  (after! mu4e-main
    (setq mu4e-main-hide-personal-addresses t))

  (after! mu4e-alert
    (mu4e-alert-set-default-style 'ignore))

  (after! mu4e-modeline
    (setq mu4e-modeline-unread-items `("U:" . ,(+mu4e-normalised-icon "nf-fa-fire"))
          mu4e-modeline-all-clear `("C:" .    ,(+mu4e-normalised-icon "nf-fa-check"))
          mu4e-modeline-all-read `("R:" .     ,(+mu4e-normalised-icon "nf-fa-check"))
          mu4e-modeline-new-items `("N:" .    ,(+mu4e-normalised-icon "nf-fa-fire")))))

(setq +org-capture-emails-file "tasks.org")

(set-email-account! "Gmail"
                    '((mu4e-sent-folder       . "/gmail/sent")
                      (mu4e-drafts-folder     . "/gmail/drafts")
                      (mu4e-trash-folder      . "/gmail/trash")
                      (mu4e-refile-folder     . "/gmail/All Mail")
                      (smtpmail-smtp-user     . "benny.ouattara@gmail.com")
                      (smtpmail-smtp-server   . "smtp.gmail.com")
                      (smtpmail-smtp-service  . 465)
                      (smtpmail-stream-type   . ssl)
                      (user-mail-address      . "benny.ouattara@gmail.com") ;; only needed for mu < 1.4
                      )
                    t)

(set-email-account! "Protonmail"
                    '((mu4e-sent-folder       . "/protonmail/sent")
                      (mu4e-drafts-folder     . "/protonmail/drafts")
                      (mu4e-trash-folder      . "/protonmail/trash")
                      (mu4e-refile-folder     . "/protonmail/All Mail")
                      (smtpmail-smtp-user     . "benny.ouattara@protonmail.com")
                      (smtpmail-smtp-server   . "127.0.0.1")
                      (smtpmail-smtp-service  . 1025)
                      (smtpmail-stream-type   . starttls)
                      (user-mail-address      . "benny.ouattara@protonmail.com")    ;; only needed for mu < 1.4
                      )
                    t)

(set-email-account! "Spotify"
                    '((mu4e-sent-folder       . "/spotify/sent")
                      (mu4e-drafts-folder     . "/spotify/drafts")
                      (mu4e-trash-folder      . "/spotify/trash")
                      (mu4e-refile-folder     . "/spotify/All Mail")
                      (smtpmail-smtp-user     . "zangao@spotify.com")
                      (smtpmail-smtp-server   . "smtp.gmail.com")
                      (smtpmail-smtp-service  . 465)
                      (smtpmail-stream-type   . ssl)
                      (user-mail-address      . "zangao@spotify.com")    ;; only needed for mu < 1.4
                      )
                    t)

(setq mu4e-bookmarks
      '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
        (:name "Today's messages" :query "date:today..now" :key 116)
        (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
        (:name "Messages with images" :query "mime:image/*" :key 112)))

(after! mu4e
  (set-popup-rule! (regexp-quote mu4e-main-buffer-name) :actions :ignore t)
  (set-popup-rule! (regexp-quote mu4e-headers-buffer-name) :actions :ignore t))

(after! mu4e-compose
  (add-hook! 'mu4e-compose-mode-hook (auto-fill-mode -1)))

(after! notmuch                                                                                                                                    
  (setq +notmuch-sync-backend 'mbsync                                                                                                              
        notmuch-show-log nil                                                                                                                       
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches                                                                               
                                 notmuch-hello-insert-alltags)                                                                                     
        notmuch-message-headers-visible nil)

  (setq sendmail-program (executable-find "msmtp")
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)

  (setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html")) ;; prefer HTML                                                       
  (setq shr-max-image-proportion 0.6)       ;; limit image size
  (setq shr-color-visible-luminance-min 60) ;; improve readability in dark themes
  (setq shr-use-colors nil) ;; ignore email colors, use your theme instead

  (setq notmuch-saved-searches
        '((:name "inbox"             :query "tag:inbox and tag:unread"        :key "i")
          (:name "gmail"             :query "tag:gmail and tag:unread"        :key "g")
          (:name "protonmail"        :query "tag:protonmail and tag:unread"   :key "p")
          (:name "jc"                :query "tag:jc and tag:unread"           :key "j")
          (:name "jfund"             :query "tag:jfund and tag:unread"        :key "f")
          (:name "today"             :query "date:today.."                    :key "t")                                                             
          (:name "week"              :query "date:7d.."                       :key "w")     
          (:name "all inbox"         :query "tag:inbox not tag:trash"         :key "I")
          (:name "all gmail"         :query "tag:gmail"                       :key "G")
          (:name "all protonmail"    :query "tag:protonmail"                  :key "P")
          (:name "all jc"            :query "tag:jc"                          :key "J")
          (:name "all jfund"         :query "tag:jfund"                       :key "F")
          (:name "sent"              :query "tag:sent"                        :key "t")
          (:name "drafts"            :query "tag:draft"                       :key "d")))

  (set-popup-rule! "^\\*notmuch" :ignore t)

  (add-hook 'notmuch-show-hook
            (lambda () (notmuch-show-tag-all '("-unread"))))

  (defun beno-notmuch-mark-read ()
    "Remove unread tag from current message or thread."
    (interactive)
    (if (eq major-mode 'notmuch-show-mode)
        (notmuch-show-tag '("-unread"))
      (notmuch-search-tag '("-unread"))))

  (defun beno-notmuch-show-in-browser ()
    "Open current message in external browser."
    (interactive)
    (notmuch-show-view-all-mime-parts))

  (evil-define-key 'normal notmuch-hello-mode-map
    "i" (cmd! (notmuch-search "tag:inbox and tag:unread"))
    "g" (cmd! (notmuch-search "tag:gmail and tag:unread"))
    "p" (cmd! (notmuch-search "tag:protonmail and tag:unread"))
    "j" (cmd! (notmuch-search "tag:jc and tag:unread"))
    "f" (cmd! (notmuch-search "tag:jfund and tag:unread"))
    "t" (cmd! (notmuch-search "date:today.."))
    "w" (cmd! (notmuch-search "date:7d.."))
    "I" (cmd! (notmuch-search "tag:inbox not tag:trash"))
    "G" (cmd! (notmuch-search "tag:gmail"))
    "P" (cmd! (notmuch-search "tag:protonmail"))
    "J" (cmd! (notmuch-search "tag:jc"))
    "F" (cmd! (notmuch-search "tag:jfund")))

  (map! :localleader
        :map (notmuch-hello-mode-map notmuch-search-mode-map
                                     notmuch-tree-mode-map notmuch-show-mode-map)
        :desc "Compose email"   "c" #'+notmuch/compose
        :desc "Sync email"      "u" #'+notmuch/update
        :desc "Quit notmuch"    "q" #'+notmuch/quit
        :desc "Mark as read"    "r" #'beno-notmuch-mark-read
        :desc "Toggle tag"      "t" #'notmuch-tag
        :map notmuch-search-mode-map
        :desc "Mark as deleted" "d" #'+notmuch/search-delete
        :desc "Mark as spam"    "s" #'+notmuch/search-spam
        :desc "Archive"         "a" (cmd! (notmuch-search-tag '("-inbox")) (notmuch-search-next-thread))
        :desc "Reply"           "R" #'notmuch-search-reply-to-thread-sender
        :map notmuch-tree-mode-map
        :desc "Mark as deleted" "d" #'+notmuch/tree-delete
        :desc "Mark as spam"    "s" #'+notmuch/tree-spam
        :desc "Archive"         "a" (cmd! (notmuch-tree-tag '("-inbox")) (notmuch-tree-next-message))
        :map notmuch-show-mode-map
        :desc "Archive"         "a" (cmd! (notmuch-show-tag '("-inbox")) (notmuch-show-next-open-message))
        :desc "Reply"           "R" #'notmuch-show-reply-sender
        :desc "Reply all"       "A" #'notmuch-show-reply
        :desc "Forward"         "f" #'notmuch-show-forward-message))

(after! (dired dired-single)
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory)
  (map! :after dired-single
        :map dired-mode-map
        :n "h" 'dired-single-up-directory
        :n "l" 'dired-single-buffer))

(setq eshell-history-size nil)

(defun beno--valid-json-p (str)
  "Return non-nil if STR is valid JSON."
  (ignore-errors (json-read-from-string str) t))

(defvar beno--eshell-output-region (cons nil nil)
  "Tracks (beg . end) of accumulated eshell output.")

(defun beno--eshell-json-print ()
  "Pretty-print JSON output in eshell."
  (let ((start (marker-position eshell-last-output-start))
        (end (marker-position eshell-last-output-end)))
    (if (not (s-matches? eshell-prompt-regexp (buffer-substring start end)))
        ;; Still accumulating output
        (progn
          (unless (car beno--eshell-output-region)
            (setcar beno--eshell-output-region start))
          (setcdr beno--eshell-output-region end))
      ;; Prompt appeared — try to pretty-print, then reset
      (unwind-protect
          (let ((beg (car beno--eshell-output-region))
                (end (cdr beno--eshell-output-region)))
            (when (and beg end (beno--valid-json-p (buffer-substring beg end)))
              (json-pretty-print beg end)))
        (setcar beno--eshell-output-region nil)
        (setcdr beno--eshell-output-region nil)))))

(after! eshell
  (add-to-list 'eshell-output-filter-functions #'beno--eshell-json-print))

(defvar beno--eshell-popup-buffer "*doom:eshell-popup:main*")

(defun beno--eshell-toggle-right (&optional arg command)
  "Toggle eshell popup to the right.
With prefix ARG, reset the eshell buffer."
  (interactive "P")
  (let* ((buf (get-buffer-create beno--eshell-popup-buffer))
         (win (get-buffer-window buf))
         (confirm-kill-processes nil))
    (cond
     ;; Visible — close it (reset first if prefix arg)
     (win
      (when (and arg (buffer-live-p buf))
        (with-current-buffer buf
          (fundamental-mode)
          (erase-buffer)))
      (delete-window win)
      (ignore-errors (kill-buffer buf)))
     ;; Not visible — open it
     (t
      (with-current-buffer buf
        (doom-mark-buffer-as-real-h)
        (if (eq major-mode 'eshell-mode)
            (run-hooks 'eshell-mode-hook)
          (eshell-mode))
        (when command
          (+eshell-run-command command buf)))
      (pop-to-buffer buf)))))

(fringe-mode 0)
(setq-default line-spacing 0.24)
(setq-default window-divider-default-right-width 24
              window-divider-default-places 'right-only)
(window-divider-mode 1)
(setq default-frame-alist
      (append default-frame-alist
              '((internal-border-width . 24)
                (left-fringe . 0)
                (right-fringe . 0))))

(use-package! nano-theme
  :hook (after-init . nano-light) ;; swap to nano-dark if preferred
  :config
  (custom-set-faces
   ;; Flyspell
   `(flyspell-incorrect ((t (:underline (:color ,nano-light-salient :style line)))))
   `(flyspell-duplicate ((t (:underline (:color ,nano-light-salient :style line)))))
   ;; Git gutter
   `(git-gutter:modified ((t (:foreground ,nano-light-salient))))
   `(git-gutter-fr:added ((t (:foreground ,nano-light-popout))))
   `(git-gutter-fr:modified ((t (:foreground ,nano-light-salient))))
   ;; LSP UI
   `(lsp-ui-doc-url:added ((t (:background ,nano-light-highlight))))
   `(lsp-ui-doc-background:modified ((t (:background ,nano-light-highlight))))
   ;; Vterm
   `(vterm-color-red ((t (:foreground ,nano-light-critical))))
   `(vterm-color-blue ((t (:foreground ,nano-light-salient))))
   `(vterm-color-green ((t (:foreground ,nano-light-popout))))
   `(vterm-color-yellow ((t (:foreground ,nano-light-popout))))
   `(vterm-color-magenta ((t (:foreground ,nano-light-salient))))
   ;; Misc UI
   `(scroll-bar ((t (:background ,nano-light-background))))
   `(child-frame-border ((t (:foreground ,nano-light-faded))))
   ;; Avy
   `(avy-lead-face-1 ((t (:foreground ,nano-light-subtle))))
   `(avy-lead-face ((t (:foreground ,nano-light-popout :weight bold))))
   `(avy-lead-face-0 ((t (:foreground ,nano-light-salient :weight bold))))))

(after! nano-theme
  (defun nano-dark ()
    (interactive)
    (message "nano-dark is disabled")))

(defun beno-enable-theme-h (theme)
  "Make window dividers match the background color."
  (let ((bg (face-background 'default)))
    (set-face-foreground 'window-divider bg)
    (set-face-foreground 'window-divider-first-pixel bg)
    (set-face-foreground 'window-divider-last-pixel bg)))

(add-hook 'enable-theme-functions #'beno-enable-theme-h)

(after! avy
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?D avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?Z avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?H avy-dispatch-alist) 'avy-action-helpful
        (alist-get ?\; avy-dispatch-alist) 'avy-action-embark))

(auto-save-visited-mode 1)
(setq auto-save-visited-interval 1)

(custom-set-faces!
  '(wgrep-face :background "#aceaac" :foreground "#004c00"))

(setq
 secrets-dir (concat sync-dir "secrets/")
 onyx-secrets (concat secrets-dir "onyx/authinfo.gpg")
 oryx-secrets (concat secrets-dir "oryx/authinfo.gpg")
 kite-secrets (concat secrets-dir "kite/authinfo.gpg"))

(pcase (system-name)
  ("onyx" (pushnew! auth-sources onyx-secrets))
  ("oryx" (pushnew! auth-sources oryx-secrets))
  ("kite" (pushnew! auth-sources kite-secrets)))

(defun beno-read-db-password (db)
  (when-let ((result (auth-source-search :database db)))
    (funcall (plist-get  (car result) :secret))))

(defun beno-sql-authenticator (wallet product user server database port)
  (beno-read-db-password database))

(setq local-wallet (pcase (system-name)
                     ("onyx" onyx-secrets)
                     ("oryx" oryx-secrets)
                     ("kite" kite-secrets)))

(after! sql
  (setq
   setcheckerpwd (beno-read-db-password "setchecker_runs")
   localpwd (beno-read-db-password "localdb")
   jazapwd (beno-read-db-password "jazadb")
   sql-password-search-wallet-function #'beno-sql-authenticator
   sql-password-wallet local-wallet
   sql-connection-alist `(("setchecker-cloudsql-connection"
                           (sql-product 'postgres)
                           (sql-user "postgres")
                           ;; password reading is done through pgpass since psql cli does't support password passing
                           ;; this line just makes sure that sql.el doesn't ask us for a dummy password
                           (sql-password ,setcheckerpwd)
                           (sql-database "setchecker_runs")
                           (sql-server "localhost")
                           (sql-port 5432))
                          ("local-postgres-connection"
                           (sql-product 'postgres)
                           (sql-user "localdb")
                           (sql-password ,localpwd)
                           (sql-database "localdb")
                           (sql-server "localhost")
                           (sql-port 5432))
                          ("jaza-postgres-connection"
                           (sql-product 'postgres)
                           (sql-user "jazadb")
                           (sql-password ,jazapwd)
                           (sql-database "jazadb")
                           (sql-server "localhost")
                           (sql-port 5432)))
   sql-postgres-login-params '(user password database server)))

(setq
 projectile-project-search-path '(("~/Code/" . 1)
                                  ("~/common-lisp" . 1)
                                  ("~/Code/archives/Code" . 1)))

(defvar beno-popup-rules-large-p nil
  "Track current display size to avoid redundant reapplication.")

(defun beno-apply-popup-rules (&rest _)
  "Apply popup rules based on current display width."
  (let ((large-p (> (display-pixel-width) 1600)))
    (unless (eq large-p beno-popup-rules-large-p)
      (setq beno-popup-rules-large-p large-p)
      (let ((size (if large-p 0.40 0.35))
            (side (if large-p 'right 'bottom)))

        ;; Default for all popups
        (set-popup-rules!
          `(("^\\*" :size ,size :vslot -4 :select t :quit nil :ttl t :side ,side)))

        ;; Ephemeral output - auto-dismiss
        (set-popup-rule! "^\\*\\(Async \\)?Shell Command"
          :size size :vslot -4 :select t :quit t :ttl t :side side)

        ;; Persistent - no timeout
        (dolist (pattern '("^\\*ChatGPT" "^\\*Ollama"))
          (set-popup-rule! pattern :size size :vslot -4
            :select t :quit nil :ttl nil :side side))

        ;; Journal/org - large display only
        (when large-p
          (dolist (pattern '("[0-9]+-[0-9]+-[0-9]+.org"
                             "journal.org"))
            (set-popup-rule! pattern :size 0.40 :vslot -4 :select t
              :quit 'other :ttl 5 :side 'right :autosave t)))))))

(add-hook 'doom-init-ui-hook #'beno-apply-popup-rules)
(add-hook 'move-frame-functions #'beno-apply-popup-rules)
(add-hook 'window-size-change-functions #'beno-apply-popup-rules)

(add-to-list 'default-frame-alist '(undecorated . t))

(use-package! info-colors
  :after info
  :hook (Info-selection . info-colors-fontify-node)
  :config
  (set-popup-rule! "^\\*info\\*" :ignore t))

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(use-package! modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts nil
        modus-themes-variable-pitch-ui nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi)

        ;; Headings — per-level customization
        modus-themes-headings
        '((1 . (overline background 1.4))
          (2 . (background 1.3))
          (3 . (bold 1.2))
          (t . (semilight 1.1)))

        ;; Completion styling
        modus-themes-completions
        '((matches . (extrabold))
          (selection . (semibold italic)))

        ;; Org source blocks
        modus-themes-org-blocks 'gray-background

        ;; Palette overrides — subtle UI tweaks
        modus-themes-common-palette-overrides
        '((fringe unspecified)
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)))

  ;; Load your preferred variant
  (load-theme 'modus-vivendi :no-confirm))

;; Toggle between light/dark
(map! :leader
      :desc "toggle modus theme" "t m" #'modus-themes-toggle)

(after! catppuccin-theme
  (setq catppuccin-flavor 'mocha))

(add-to-list 'load-path  "~/Code/dotfiles/lib/")
(require 'soccer)
(map! :leader
      (:prefix-map ("o" . "open")
                   (:prefix ("S" . "soccer")
                    :desc "Favorite fixtures" "S" #'list-soccer-fixtures
                    :desc "League fixtures" "s" #'list-league-fixtures
                    :desc "Followed leagues" "l" #'list-soccer-leagues
                    :desc "Followed teams" "t" #'list-soccer-teams
                    :desc "Teams fixtures" "T" #'list-soccer-team-fixtures
                    :desc "Follow league" "f" #'soccer-follow-league
                    :desc "Unfollow league" "U" #'soccer-unfollow-league
                    :desc "Unfollow team" "u" #'soccer-unfollow-team
                    :desc "Follow team" "F" #'soccer-follow-team)))

(after! clojure-mode
  (defun jazacash-cycle-slice-file ()                                             
    "Cycle through existing core/db/routes/view/worker files for the current
  feature."                                                                       
    (interactive) 
    (let* ((file (buffer-file-name))                                              
           (siblings '("core" "db" "routes" "view" "worker"))
           (current (file-name-base file))                                        
           (dir (file-name-directory file))
           (candidates (cdr (member current siblings)))                           
           (found (seq-find (lambda (s) (file-exists-p (concat dir s ".clj")))
                            (append candidates siblings))))                       
      (if found                                                                   
          (find-file (concat dir found ".clj"))                                   
        (message "No other slice files found"))))                                 

  (defun bb! (cmd)
    (let ((default-directory (projectile-project-root)))                          
      (compile cmd)))
  
  (define-clojure-indent
   (ex/try!  1)
   (ex/catch 2)
   (jui/form 'defun))

  (map! :map clojure-mode-map                                                   
        :localleader "TAB" #'jazacash-cycle-slice-file
        (:prefix ("b" . "bb")
         :desc "Test unit"  "u" (cmd! (bb! "bb test unit"))                     
         :desc "Test all"   "a" (cmd! (bb! "bb test all"))
         :desc "Lint"       "l" (cmd! (bb! "bb lint"))                          
         :desc "Format"     "f" (cmd! (bb! "bb fmt"))
         :desc "Prep"       "p" (cmd! (bb! "bb prep")))))

(after! cider
  ;; (add-hook 'before-save-hook 'cider-format-buffer t t)
  (defun beno-portal-open ()
    (interactive)
    (cider-nrepl-sync-request:eval
     "(do
         (ns dev)
         (def portal ((requiring-resolve 'portal.api/open) {:theme :portal.colors/material-ui}))
         (add-tap (requiring-resolve 'portal.api/submit)))"))

  (defun beno-portal-clear ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/clear)"))

  (defun beno-portal-close ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/close)"))

  (map! :map clojure-mode-map
        :localleader
        :desc "open portal"  :n "o" #'beno-portal-open
        :desc "close portal" :n "q" #'beno-portal-close
        :desc "clear portal" :n "l" #'beno-portal-clear
        (:prefix ("t" . "test")
         :desc "Run ns tests"     "t"  #'cider-test-run-ns-tests                 
         :desc "Run test at point" "f" #'cider-test-run-test-at-point           
         :desc "Rerun failed"     "r"  #'cider-test-rerun-failed-tests           
         :desc "Jump to test"     "j"  #'projectile-toggle-between-implementation-and-test))

  (setq cider-clojure-cli-aliases ":portal")
  
  (map! :map (list clojure-mode-map clojurec-mode-map clojurescript-mode-map)
        :localleader                                                            
        (:prefix ("S" . "system")
         :desc "Start"    "s" (cmd! (cider-interactive-eval "(user/start)"))    
         :desc "Stop"     "S" (cmd! (cider-interactive-eval "(user/stop)"))
         :desc "Restart"  "r" (cmd! (cider-interactive-eval "(user/restart)"))
         :desc "Connect CLJ"  "c" (cmd! (cider-connect '(:host "localhost" :port 7004)))        
         :desc "Connect CLJS" "C" (cmd! (cider-connect-cljs '(:host "localhost" :port 7002 :cljs-repl-type shadow)))                                                  
         :desc "Hot reload" "R" #'cider-ns-refresh)
        (:prefix ("D" . "dev db")
         :desc "Seed"     "s" (cmd! (cider-interactive-eval "(user/seed!)"))    
         :desc "Migrate"  "m" (cmd! (cider-interactive-eval "(user/migrate)"))
         :desc "Rollback" "r" (cmd! (cider-interactive-eval "(user/rollback)")) 
         :desc "Reset"    "R" (cmd! (cider-interactive-eval "(user/reset-db)"))
         :desc "Start Tasks" "t" (cmd! (cider-interactive-eval "(user/start-tasks)"))
         :desc "Stop Tasks"  "T" (cmd! (cider-interactive-eval "(user/stop-tasks)"))
         :desc "Reload queries" "q" (cmd! (cider-interactive-eval "(user/reload-queries)")))))

(defun beno-find-file-in-dotfiles ()
  "Search for a file in `dotfiles'."
  (interactive)
  (doom-project-find-file "~/Code/dotfiles"))

(map! :leader
      :desc "find in dotfiles"      "f d" #'beno-find-file-in-dotfiles
      :desc "find in private config" "f p" #'doom/find-file-in-private-config)

(defun beno-recenter (&rest _)
  (evil-scroll-line-to-center nil))

(dolist (fn '(consult-line
              evil-ex-search-word-forward
              evil-ex-search-next
              evil-ex-search-previous))
  (advice-add fn :after #'beno-recenter))

(after! vterm
  (setq vterm-clear-scrollback-when-clearing t
        vterm-shell "/bin/zsh"))

(defun beno-sync-notmuch ()
  (interactive)
  (async-shell-command "mbsync --all && notmuch new && afew --tag --new"))

(defun beno-kite-up ()
  (interactive)
  (async-shell-command "up kite!"))

(map! :leader
      (:prefix ("j" . "system")
       :desc "async shell command"  "!"   #'async-shell-command
       :desc "sync mail"            "m"   #'beno-sync-notmuch
       :desc "up kite!"             "u"   #'beno-kite-up))

(after! org
  (require 'verb)
  (map! :map org-mode-map
        :localleader
        (:prefix ("v" . "verb")
                 "v" #'verb-send-request-on-point
                 "s" #'verb-send-request-on-point-other-window
                 "f" #'verb-send-request-on-point-other-window-stay
                 "k" #'verb-kill-all-response-buffers
                 "e" #'verb-export-request-on-point-curl
                 "u" #'verb-export-request-on-point-verb
                 "b" #'verb-export-request-on-point-browse-url)))
