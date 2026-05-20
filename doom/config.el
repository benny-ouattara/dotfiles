(require 'f)
(require 's)
(require 'dash)

(setq
 user-full-name "Ben O."
 user-mail-address "benny.ouattara@gmail.com")

(setq
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-agenda-custom-commands
 '(("d" "Daily review"
    ((agenda "" ((org-agenda-span 'day)
                 (org-super-agenda-groups
                  '((:name "Schedule" :time-grid t)
                    (:name "Due today" :deadline today)
                    (:name "Overdue" :deadline past)
                    (:name "Due soon" :deadline future)))))
     (todo "STRT" ((org-agenda-overriding-header "In Progress")))
     (todo "WAIT" ((org-agenda-overriding-header "Waiting On")))
     (todo "HOLD" ((org-agenda-overriding-header "On Hold")))))
   ("p" "Projects" tags-todo "+LEVEL=3"
    ((org-agenda-files '("~/Sync/org/projects.org"))
     (org-agenda-overriding-header "All Project Tasks")))
   ("w" "Weekly review"
    ((agenda "" ((org-agenda-span 'week)
                 (org-super-agenda-groups
                  '((:name "This week" :time-grid t)
                    (:name "Due" :deadline future)
                    (:name "Overdue" :deadline past)))))
     (todo "STRT" ((org-agenda-overriding-header "In Progress")))
     (todo "WAIT" ((org-agenda-overriding-header "Waiting On")))))
   ("h" "People" tags-todo "people"
    ((org-agenda-overriding-header "People Management")
     (org-super-agenda-groups
      '((:name "Today" :scheduled today)
        (:name "This week" :scheduled future)
        (:name "Recurring" :anything t)))))
   ("o" "OKRs" tags-todo "okr"
    ((org-agenda-overriding-header "OKR Tracking")
     (org-agenda-files '("~/Sync/org/okrs.org" "~/Sync/org/tasks.org")))))
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

(setq
 org-startup-folded 'show2levels
 org-auto-align-tags nil
 sync-dir "~/Sync/"
 org-directory (concat sync-dir "org")
 org-agenda-files (ignore-errors (directory-files org-directory t "\\.org$" t))
 org-hide-emphasis-markers t
 org-tags-column -80
 org-refile-targets '(("projects.org" :maxlevel . 3)
                      ("tasks.org" :maxlevel . 2)
                      ("notes.org" :maxlevel . 2)
                      ("okrs.org" :maxlevel . 3))
 +org-capture-todo-file "tasks.org")

(after! org
  (setq org-capture-templates
        (append org-capture-templates
                '(("e" "Email task" entry
                   (file+headline +org-capture-todo-file "Email")
                   "* TODO %? :email:\nFrom: %a\n%U"
                   :empty-lines 1)
                  ("n" "Email note" entry
                   (file+headline "notes.org" "Inbox")
                   "* %? :email:\nFrom: %a\n%U"
                   :empty-lines 1)
                  ("P" "Project task" entry
                   (file+headline "projects.org" "Jazacash")
                   "* TODO %?\n%U"
                   :empty-lines 1)
                  ("s" "Scheduled task" entry
                   (file+headline +org-capture-todo-file "Inbox")
                   "* TODO %?\nSCHEDULED: %^t\n%U"
                   :empty-lines 1)
                  ("d" "Deadline task" entry
                   (file+headline +org-capture-todo-file "Inbox")
                   "* TODO %?\nDEADLINE: %^t\n%U"
                   :empty-lines 1)
                  ("1" "1-1 note" entry
                   (function beno-capture-person-node)
                   "* %U 1-1\n** Updates\n%?\n** Blockers\n\n** Priorities\n\n** Action Items\n"
                   :empty-lines 1)
))))

(defun beno-capture-person-node ()
  "Navigate to a person's 1-1 Meeting Notes heading for capture."
  (let* ((node (org-roam-node-read nil
                (lambda (node)
                  (member "people" (org-roam-node-tags node)))))
         (file (org-roam-node-file node)))
    (set-buffer (org-capture-target-buffer file))
    (goto-char (point-min))
    (unless (re-search-forward "^\\*\\* 1-1 Meeting Notes" nil t)
      (goto-char (point-max))
      (insert "\n** 1-1 Meeting Notes\n"))
    (org-narrow-to-subtree)))

(after! org-journal
  (setq org-journal-enable-agenda-integration t)
  (map! :leader :desc "Open current journal" "k" #'org-journal-open-current-journal-file))

(map! :leader
      :desc "split with eshell"     ">" #'beno--eshell-toggle-right
      :desc "fuzzy search visible"  "a" #'evil-avy-goto-char-2
      :desc "line in visible"       "A" #'avy-goto-line
      :desc "open file other win"   "V" #'projectile-find-file-other-window
      :desc "open buffer other win" "v" #'switch-to-buffer-other-window)

(map! "C-s" #'consult-line)

(map! :nvi "C-1" #'+workspace/switch-to-0                                                                                        
      :nvi "C-2" #'+workspace/switch-to-1                                                                                        
      :nvi "C-3" #'+workspace/switch-to-2                                                                                        
      :nvi "C-4" #'+workspace/switch-to-3
      :nvi "C-5" #'+workspace/switch-to-4                                                                                        
      :nvi "C-6" #'+workspace/switch-to-5
      :nvi "C-7" #'+workspace/switch-to-6
      :nvi "C-8" #'+workspace/switch-to-7
      :nvi "C-9" #'+workspace/switch-to-8)

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

(after! notmuch
  ;; Re-apply popup rules to override Doom's notmuch module defaults
  (setq beno-popup-rules-large-p 'reset)
  (beno-apply-popup-rules)

  (setq +notmuch-sync-backend 'mbsync
        notmuch-show-log nil
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        notmuch-hello-auto-refresh t
        notmuch-message-headers-visible nil
        notmuch-always-prompt-for-sender nil
        notmuch-identities '("Ben A. <benny.ouattara@gmail.com>"
                             "Ben A. <ben.abubaker@proton.me>"
                             "Jazafund <jazafund@proton.me>"
                             "Jaza Support <support@jaza.cash>"
                             "Jaza Ops <ops@jaza.cash>"
                             "Jaza Compliance <compliance@jaza.cash>"
                             "Jaza Info <info@jaza.cash>"
                             "Jaza Fraud <fraud@jaza.cash>"
                             "Jaza HR <hr@jaza.cash>"
                             "Jaza Sales <sales@jaza.cash>"
                             "Jaza System <system@jaza.cash>"))

  (setq sendmail-program (executable-find "msmtp")
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)

  (setq notmuch-fcc-dirs '(("benny.ouattara@gmail.com"  . "gmail/sent +sent")
                            ("ben.abubaker@proton.me"   . "protonmail/sent +sent")
                            ("jazafund@proton.me"       . "jfund/sent +sent")
                            ("support@jaza.cash"        . "jc-support/sent +sent")
                            ("ops@jaza.cash"            . "jc-ops/sent +sent")
                            ("compliance@jaza.cash"     . "jc-compliance/sent +sent")
                            ("info@jaza.cash"           . "jc-info/sent +sent")
                            ("fraud@jaza.cash"          . "jc-fraud/sent +sent")
                            ("hr@jaza.cash"             . "jc-hr/sent +sent")
                            ("sales@jaza.cash"          . "jc-sales/sent +sent")
                            ("system@jaza.cash"         . "jc-system/sent +sent")))

  (setq notmuch-address-command 'internal
        notmuch-address-internal-completion '(sent received nil))

  (notmuch-address-setup)

  (defun beno-notmuch-address-capf ()
    "Completion-at-point for notmuch addresses, only in address headers."
    (when (and (save-excursion
                (beginning-of-line)
                (re-search-forward "^\\(To\\|Cc\\|Bcc\\): " (line-end-position) t))
              (<= (match-end 0) (point)))
      (let* ((end (point))
             (beg (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
        (list beg end
              (completion-table-dynamic
               (lambda (prefix)
                 (notmuch-address-options prefix)))))))

  (add-hook 'notmuch-message-mode-hook
            (lambda ()
              (auto-fill-mode -1)
              (visual-line-mode 1)
              (add-hook 'completion-at-point-functions
                        #'beno-notmuch-address-capf nil t)))

  ;; Fcc saves sent mail locally; regular sync pushes to remote

  (defun beno-check-attachment ()
    "Warn if message mentions attachment but has none."
    (interactive)
    (let ((body (save-excursion
                  (message-goto-body)
                  (buffer-substring-no-properties (point) (point-max)))))
      (when (and (string-match-p "\\battach\\(ed\\|ment\\|ing\\)\\b" body)
                 (not (string-match-p "<#part " body)))
        (unless (y-or-n-p "No attachment found. Send anyway? ")
          (user-error "Aborted")))))

  (add-hook 'message-send-hook #'beno-check-attachment)

  (setq mml-default-directory "~/Downloads/"
        mm-default-directory "~/Downloads/")

  (defun beno-notmuch-save-all-attachments ()
    "Save all attachments from the current message."
    (interactive)
    (let ((dir (read-directory-name "Save attachments to: " "~/Downloads/"))
          (count 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\[ .+: \\(application\\|image\\|audio\\|video\\)/.+ \\]" nil t)
          (goto-char (match-beginning 0))
          (notmuch-show-apply-to-current-part-handle
           (lambda (handle)
             (let* ((name (or (mm-handle-filename handle)
                              (format "attachment-%d" (cl-incf count))))
                    (path (expand-file-name name dir)))
               (mm-save-part-to-file handle path)
               (cl-incf count))))
          (goto-char (match-end 0))))
      (message "Saved %d attachment(s) to %s" count dir)))

  (defun beno-dired-attach-files ()
    "Attach marked files in dired to the current compose buffer."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (other-window 1)
      (goto-char (point-max))
      (dolist (f files)
        (mml-attach-file f (mm-default-file-type f) nil "attachment"))
      (message "Attached %d file(s)" (length files))))

  (add-hook 'notmuch-message-mode-hook
            (lambda ()
              (when (fboundp 'dnd-protocol-alist)
                (setq-local dnd-protocol-alist
                            (cons '("^file:" . beno-dnd-attach-file) dnd-protocol-alist)))))

  (defun beno-dnd-attach-file (uri _action)
    "Attach a dragged file to the compose buffer."
    (let ((file (dnd-get-local-file-name uri t)))
      (when file
        (goto-char (point-max))
        (mml-attach-file file (mm-default-file-type file) nil "attachment")
        'private)))

  (setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html")) ;; prefer HTML                                                       
  (setq shr-max-image-proportion 0.6)       ;; limit image size
  (setq shr-color-visible-luminance-min 60) ;; improve readability in dark themes
  (setq shr-use-colors nil) ;; ignore email colors, use your theme instead
  (setq shr-use-fonts nil)  ;; use doom font instead of variable-pitch
  (setq shr-max-width 100)  ;; keep email body readable, not too wide

  (setq notmuch-saved-searches
        '((:name "inbox"             :query "tag:inbox and tag:unread"        :key "i" :count-query "tag:inbox and tag:unread")
          (:name "gmail"             :query "tag:gmail and tag:unread"        :key "g" :count-query "tag:gmail and tag:unread")
          (:name "protonmail"        :query "tag:protonmail and tag:unread"   :key "p" :count-query "tag:protonmail and tag:unread")
          (:name "jc"                :query "tag:jc and tag:unread"           :key "j" :count-query "tag:jc and tag:unread")
          (:name "jfund"             :query "tag:jfund and tag:unread"        :key "f" :count-query "tag:jfund and tag:unread")
          (:name "today"             :query "date:today.."                    :key "t")
          (:name "week"              :query "date:7d.."                       :key "w")
          (:name "all inbox"         :query "tag:inbox not tag:trash"         :key "I")
          (:name "all gmail"         :query "tag:gmail"                       :key "G")
          (:name "all protonmail"    :query "tag:protonmail"                  :key "P")
          (:name "all jc"            :query "tag:jc"                          :key "J")
          (:name "all jfund"         :query "tag:jfund"                       :key "F")
          (:name "sent"              :query "tag:sent"                        :key "e")
          (:name "flagged"           :query "tag:flagged"                     :key "x" :count-query "tag:flagged")
          (:name "drafts"            :query "tag:draft"                       :key "d")))

  (add-hook 'notmuch-show-hook
            (lambda () (notmuch-show-tag '("-unread"))))

  (defun beno-notmuch-match-identity (address)
    "Find the full identity string for ADDRESS from notmuch-identities."
    (cl-find-if (lambda (id) (string-match-p (regexp-quote address) id))
                notmuch-identities))

  (defun +notmuch/compose ()
    "Compose new mail, prompting for identity."
    (interactive)
    (let ((from (completing-read "From: " notmuch-identities nil t)))
      (notmuch-mua-mail nil nil (list (cons 'From from)))))

  (advice-add 'notmuch-mua-reply :around
              (lambda (orig-fn &rest args)
                (apply orig-fn args)
                (let* ((from (message-field-value "from"))
                       (addr (and from (cadr (mail-extract-address-components from))))
                       (identity (and addr (beno-notmuch-match-identity addr))))
                  (when identity
                    (message-replace-header "From" identity)))))

  (defun beno-notmuch-mark-read ()
    "Remove unread tag from current message or thread."
    (interactive)
    (if (eq major-mode 'notmuch-show-mode)
        (notmuch-show-tag '("-unread"))
      (notmuch-search-tag '("-unread"))))

  (evil-define-key 'normal notmuch-hello-mode-map
    "c" #'+notmuch/compose
    "C" #'+notmuch/compose
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
    "F" (cmd! (notmuch-search "tag:jfund"))
    "e" (cmd! (notmuch-search "tag:sent"))
    "x" (cmd! (notmuch-search "tag:flagged")))

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
        :desc "Undo delete/spam" "u" (cmd! (notmuch-search-tag '("+inbox" "-deleted" "-spam")) (notmuch-search-next-thread))
        :desc "Toggle flag"     "x" (cmd! (notmuch-search-tag (if (member "flagged" (notmuch-search-get-tags)) '("-flagged") '("+flagged"))))
        :desc "Archive"         "a" (cmd! (notmuch-search-tag '("-inbox")) (notmuch-search-next-thread))
        :desc "Reply"           "R" #'notmuch-search-reply-to-thread-sender
        :map notmuch-tree-mode-map
        :desc "Mark as deleted" "d" #'+notmuch/tree-delete
        :desc "Mark as spam"    "s" #'+notmuch/tree-spam
        :desc "Toggle flag"     "x" (cmd! (notmuch-tree-tag (if (member "flagged" (notmuch-tree-get-tags)) '("-flagged") '("+flagged"))))
        :desc "Archive"         "a" (cmd! (notmuch-tree-tag '("-inbox")) (notmuch-tree-next-message))
        :map notmuch-show-mode-map
        :desc "Mark as deleted" "d" (cmd! (notmuch-show-tag '("+deleted" "-inbox")) (notmuch-show-next-open-message))
        :desc "Mark as spam"    "s" (cmd! (notmuch-show-tag '("+spam" "-inbox")) (notmuch-show-next-open-message))
        :desc "Undo delete/spam" "u" (cmd! (notmuch-show-tag '("+inbox" "-deleted" "-spam")))
        :desc "Toggle flag"     "x" (cmd! (notmuch-show-tag (if (member "flagged" (notmuch-show-get-tags)) '("-flagged") '("+flagged"))))
        :desc "Archive"         "a" (cmd! (notmuch-show-tag '("-inbox")) (notmuch-show-next-open-message))
        :desc "Reply"           "R" #'notmuch-show-reply-sender
        :desc "Reply all"       "A" #'notmuch-show-reply
        :desc "Forward"         "f" #'notmuch-show-forward-message
        :desc "Save attachments" "S" #'beno-notmuch-save-all-attachments
        :desc "Capture task"    "e" (cmd! (org-capture nil "e"))
        :desc "Capture note"    "n" (cmd! (org-capture nil "n"))))

  (map! :after dired
        :map dired-mode-map
        :localleader
        :desc "Attach to email" "a" #'beno-dired-attach-files)

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
  :defer t
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
  '(wgrep-face :inherit diff-added))

(setq
 secrets-dir (concat sync-dir "secrets/")
 onyx-secrets (concat secrets-dir "onyx/authinfo.gpg")
 oryx-secrets (concat secrets-dir "oryx/authinfo.gpg")
 otter-secrets (concat secrets-dir "otter/authinfo.gpg")
 kite-secrets (concat secrets-dir "kite/authinfo.gpg"))

(pcase (system-name)
  ("onyx" (pushnew! auth-sources onyx-secrets))
  ("oryx" (pushnew! auth-sources oryx-secrets))
  ("otter" (pushnew! auth-sources otter-secrets))
  ("kite" (pushnew! auth-sources kite-secrets)))

(defun beno-read-db-password (db)
  (when-let ((result (auth-source-search :database db)))
    (funcall (plist-get  (car result) :secret))))

(defun beno-sql-authenticator (wallet product user server database port)
  (beno-read-db-password database))

(setq local-wallet (pcase (system-name)
                     ("onyx" onyx-secrets)
                     ("oryx" oryx-secrets)
                     ("otter" otter-secrets)
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
              :quit 'other :ttl 5 :side 'right :autosave t)))

        ;; Notmuch — master panes as regular windows
        (dolist (pattern '("^\\*notmuch-hello" "^\\*notmuch-search" "^\\*notmuch-saved-" "^\\*notmuch-tree"))
          (set-popup-rule! pattern :ignore t))

        ;; Notmuch — detail pane (Doom renames show buffers to *subject:...*)
        (set-popup-rule! "^\\*subject:" :size 0.60 :side 'right :select t :quit 'other :ttl nil)

        ;; Notmuch — compose at the bottom
        (set-popup-rule! "^\\*\\(?:unsent \\)?mail" :size 0.40 :side 'bottom :select t :quit nil :ttl nil)))))

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
  :defer t
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
          (border-mode-line-inactive bg-mode-line-inactive))))

;; Toggle between light/dark
(map! :leader
      :desc "toggle modus theme" "t m" #'modus-themes-toggle)

(after! catppuccin-theme
  (setq catppuccin-flavor 'mocha))

(use-package! soccer
  :defer t
  :load-path "~/Code/dotfiles/lib/"
  :commands (list-soccer-fixtures list-league-fixtures list-soccer-leagues
             list-soccer-teams list-soccer-team-fixtures soccer-follow-league
             soccer-unfollow-league soccer-unfollow-team soccer-follow-team)
  :init
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
                      :desc "Follow team" "F" #'soccer-follow-team))))

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

  (map! :map (list clojure-mode-map clojurec-mode-map clojurescript-mode-map)
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

  (map! :map (list clojure-mode-map clojurec-mode-map clojurescript-mode-map)
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

(map! :n "gF" #'find-file-at-point)

(map! :leader
      :desc "find in dotfiles"       "f d" #'beno-find-file-in-dotfiles
      :desc "find in private config" "f p" #'doom/find-file-in-private-config)

(defun beno-recenter (&rest _)
  (evil-scroll-line-to-center nil))

(dolist (fn '(consult-line
              evil-ex-search-word-forward
              evil-ex-search-next
              evil-ex-search-forward
              evil-ex-search-backward
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

(use-package! verb
  :defer t
  :commands (verb-send-request-on-point verb-send-request-on-point-other-window
             verb-send-request-on-point-other-window-stay verb-kill-all-response-buffers
             verb-export-request-on-point-curl verb-export-request-on-point-verb
             verb-export-request-on-point-browse-url)
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        (:prefix ("v" . "verb")
                 "v" #'verb-send-request-on-point
                 "s" #'verb-send-request-on-point-other-window
                 "f" #'verb-send-request-on-point-other-window-stay
                 "k" #'verb-kill-all-response-buffers
                 "e" #'verb-export-request-on-point-curl
                 "u" #'verb-export-request-on-point-verb
                 "b" #'verb-export-request-on-point-browse-url)))

(after! corfu
  (setq corfu-preselect 'first))

(after! eglot                                                                                                                                      
  (setq eglot-connect-timeout 300
        eglot-events-buffer-config '(:size 2000000 :format full)))
