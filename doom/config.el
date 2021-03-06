(require 'f)
(require 's)
(require 'dash)

(defun beno--org-babel-tangle-config ()
  (when (string-equal (f-filename (buffer-file-name))
                      "config.org")
    (org-babel-tangle)))

(defun beno--tangle-on-save ()
  (add-hook 'after-save-hook #'beno--org-babel-tangle-config))

(add-hook 'org-mode-hook #'beno--tangle-on-save)

(setq doom-font (font-spec :family "monaco" :size 23 :weight 'normal)
      doom-big-font (font-spec :family "monaco" :size 37)
      doom-variable-pitch-font (font-spec :family "Avenir Next" :size 18)
      org-roam-v2-ack t
      mac-command-modifier 'meta
      ns-command-modifier 'meta
      rfc-mode-directory (expand-file-name "~/rfc/")
      lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
      ;; mu-root (s-chop-suffixes '("/mu" "/bin") (file-truename  (executable-find "mu")))
      ;; mu4e-path (concat mu-root "/share/emacs/site-lisp/mu4e")
      tdlib-path "/nix/store/y27x4zzs8wm8qwskbp8y5g3gx1qkjg3m-tdlib-unstable-2020-10-25/include/td/telegram"
      user-full-name "Ben O"
      user-mail-address "benny.ouattara@gmail.com"
      confirm-kill-emacs nil
      evil-insert-state-cursor 'hbar
      project-dir "~/Code"
      project-prefix "kata"
      org-roam-capture-templates '(("d" "default" plain
                                    #'org-roam-capture--get-point
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/default.org")
                                    :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                    :head "#+title: ${title}\n#+date: %U\n"
                                    :unnarrowed t)
                                   ("l" "programming language" plain
                                    #'org-roam-capture--get-point
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/programming.org")
                                    :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                    :head "#+title: ${title}\n#+date: %U\n#+filetags: programming\n"
                                    :unnarrowed t)
                                   ("b" "book notes" plain
                                    #'org-roam-capture--get-point
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/book.org")
                                    :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                    :head "#+title: ${title}\n#+date: %U\n#+filetags: book\n"
                                    :unnarrowed t)
                                   ("p" "project" plain
                                    #'org-roam-capture--get-point
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/project.org")
                                    :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                    :head "#+title: ${title}\n#+date: %U\n#+filetags: project\n"
                                    :unnarrowed t)
                                   ("c" "code" plain
                                    #'org-roam-capture--get-point
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/code.org")
                                    :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                    :head "#+title: ${title}\n#+date: %U\n#+filetags: interview\n"
                                    :unnarrowed t))
      skeletor-project-directory project-dir
      skeletor-user-directory (concat doom-private-dir "templates")
      skeletor-completing-read-function 'ivy-read
      org-directory "~/Sync/org"
      org-spotify-directory (concat org-directory "/spotify")
      org-mail-directory (concat org-directory "/mail.org")
      home-dir (getenv "HOME")
      display-line-numbers-type 'relative
      eshell-history-size nil
      ;; NOTE: overwrites org export options
      org-mime-export-options '(:section-numbers nil
                                :with-author nil
                                :with-toc nil)
      org-agenda-skip-scheduled-if-done t
      projectile-project-search-path '("~/Code/" "~/common-lisp")
      org-ellipsis " ??? "
      org-hide-emphasis-markers t
      org-tags-column -80
      org-agenda-files (ignore-errors (directory-files org-directory t "\\.org$" t))
      org-log-done 'time
      org-pomodoro-length 45
      org-pomodoro-short-break-length 15
      org-refile-targets (quote ((nil :maxlevel . 3)))
      +doom-dashboard-banner-file (expand-file-name "splash.png" doom-private-dir)
      tramp-histfile-override "/dev/null"
      +org-capture-todo-file "tasks.org"
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

(menu-bar-mode t)
(global-prettify-symbols-mode)
(rainbow-mode)

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

(defun beno--auto-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'tsdh-light t))
    ('dark (load-theme 'doom-dracula t))))

(add-hook 'ns-system-appearance-change-functions #'beno--auto-theme)

(when (not (display-graphic-p))
  (progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (mapc #'disable-theme custom-enabled-themes)
    (setq doom-theme 'doom-
          evil-emacs-state-cursor '("red" box)
          evil-normal-state-cursor '("black" box)
          evil-visual-state-cursor '("black" box)
          evil-insert-state-cursor '("red" bar)
          evil-motion-state-cursor '("gray" box))))

;; wrap lines around in org buffers
(add-hook 'org-mode-hook #'auto-fill-mode)

(global-auto-revert-mode t)

(defun +org*update-cookies ()
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let (org-hierarchical-todo-statistics)
      (org-update-parent-todo-statistics))))

(advice-add #'+org|update-cookies :override #'+org*update-cookies)

(add-hook! 'org-mode-hook (company-mode -1))
(add-hook! 'org-mode-hook (org-bullets-mode 1))
(add-hook! 'org-capture-mode-hook (company-mode -1))

(after! org
  (set-face-attribute 'org-link nil
                      :weight 'normal
                      :background nil)
  (set-face-attribute 'org-code nil
                      :foreground "#a9a1e1"
                      :background nil)
  (set-face-attribute 'org-date nil
                      :foreground "#5B6268"
                      :background nil)
  (set-face-attribute 'org-level-1 nil
                      :foreground "Steelblue3"
                      :background nil
                      :height 1.2
                      :weight 'normal)
  (set-face-attribute 'org-level-2 nil
                      :foreground "RoyalBlue"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-3 nil
                      :foreground "DeepSkyBlue"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-4 nil
                      :foreground "DodgerBlue3"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-5 nil
                      :weight 'normal)
  (set-face-attribute 'org-level-6 nil
                      :weight 'normal)
  (set-face-attribute 'org-document-title nil
                      :foreground "SlateGray4"
                      :background nil
                      :height 1.75
                      :weight 'bold)

  (pushnew! org-capture-templates
            '("m" "Email workflow")
            '("mf" "Follow up" entry (file+olp org-mail-directory "Follow up")
              "* TODO follow up with %:fromname on %a\n\n%i"
              :immediate-finish t)
            '("mr" "Read later" entry (file+olp org-mail-directory "Read later")
              "* TODO read %:subject\n%a\n\n%i"
              :immediate-finish t)))

(use-package! org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("???" "???" "???" "???")))

(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)

(when (not (file-exists-p (concat doom-cache-dir "tramp-histfile")))
  (make-empty-file (concat doom-cache-dir "tramp-histfile")))

(defun beno--indent (n)
  (interactive "p")
  ;; applies to java, c and c++
  (setq c-basic-offset n)
  (setq coffee-tab-width n)
  (setq javascript-indent-level n)
  (setq typescript-indent-level n)
  (setq js-indent-level n)
  (setq js2-basic-offset n)
  (setq web-mode-markup-indent-offset n)
  (setq web-mode-css-indent-offset n)
  (setq web-mode-code-indent-offset n)
  (setq css-indent-offset n))

(beno--indent 2)

(defun work-window-split-three ()
  (interactive)
  "Splits frame in three. With eshell on the bottom right
and org files on the top right. Keeps current window on the left."
  (progn  (dired-other-window org-spotify-directory)
          (+eshell/split-below)))

(defun beno--eshell-split-right ()
  "Create a new eshell window 2/3 to the right of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p))
         (+eshell-enable-new-shell-on-split
          (or +eshell-enable-new-shell-on-split (frame-parameter nil 'saved-wconf))))
    (select-window (split-window-horizontally (* 2 (/ (window-total-width) 3))))
    (+eshell--bury-buffer dedicated-p)))

(map! :leader
      :desc "close current window"
      "0" #'evil-quit)

(map! :leader
      :desc "close other window"
      "9" #'delete-other-windows)

(map! :leader
      :desc "work window split"
      ">" #'beno--eshell-split-right)

(map! :leader
      :desc "left work window"
      "1" #'evil-window-top-left)

(map! :leader
      :desc "right bottom work window"
      "2" #'evil-window-bottom-right)

(map! :desc "fuzzy search visible buffer"
      :leader
      "a" #'evil-avy-goto-char-2)

(map! :leader
      :desc "open file other window"
      "V" #'projectile-find-file-other-window)

(map! :leader
      :desc "open buffer other window"
      "v" #'switch-to-buffer-other-window)

(map! "C-s" #'consult-line)

(map! :leader
      :desc "delete buffer"
      "d" #'kill-buffer)

(map! :leader
      :desc "hide in level"
      "l" #'hs-hide-level)

(map! :leader
      :desc "show block"
      "L" #'hs-show-block)

(map! :leader
      :desc "find file at point"
      "/" #'find-file-at-point)

(map! :leader
      :desc "next workspace"
      "]" #'+workspace:switch-next)

(map! :leader
      :desc "previous workspace"
      "[" #'+workspace:switch-previous)

;; setup lsp server for eglot
;; eglot doesn't recognize ~ for user home directory
;; (setq lsp-jar (concat home-dir  "/.emacs.d/.cache/lsp/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.6.0.v20200915-1508.jar"))

;; (defun set-lsp-jar ()
;;   (setenv "CLASSPATH" lsp-jar))

;; (add-hook 'java-mode-hook #'set-lsp-jar)

;; breadcrumb is a nice feature to know about, not using it now
;; (after! lsp-mode
;;   (lsp-headerline-breadcrumb-mode))

;; makes lsp-mode a little more bearable: hide all the UI noise
(setq lsp-ui-sideline-show-code-actions nil)

(setq java-dir "/Library/Java/JavaVirtualMachines")
(setq java-home-suffix "/Contents/Home")

(defun beno--switch-jvm (chosen-jvm)
  (interactive (list
                (ivy-completing-read "Choose JVM:"
                                     (-filter
                                      (lambda (filename) (and (not (equal filename "."))
                                                         (not (equal filename ".."))))
                                      (directory-files java-dir)))))
  (let ((old-env (getenv "JAVA_HOME"))
        (home-path (concat java-dir "/" chosen-jvm java-home-suffix)))
    (setenv "JAVA_HOME" home-path)))

;; TODO: refactor project creation logic in a =macro=
;; Give me a random name
(defun haikunate (token-range &optional prefix)
  "Generate random descriptive name.
A random adjective is chosen followed by a random nound and a random number."
  (let* ((adjectives '(autumn hidden bitter misty silent empty dry dark summer
                              icy delicate quiet white cool spring winter patient
                              twilight dawn crimson wispy weathered blue billowing
                              broken cold damp falling frosty green long late lingering
                              bold little morning muddy old red rough still small
                              sparkling throbbing shy wandering withered wild black
                              young holy solitary fragrant aged snowy proud floral
                              restless divine polished ancient purple lively nameless))
         (nouns '(waterfall river breeze moon rain wind sea morning
                            snow lake sunset pine shadow leaf dawn glitter forest
                            hill cloud meadow sun glade bird brook butterfly
                            bush dew dust field fire flower firefly feather grass
                            haze mountain night pond darkness snowflake silence
                            sound sky shape surf thunder violet water wildflower
                            wave water resonance sun wood dream cherry tree fog
                            frost voice paper frog smoke star))
         (adjective (seq-random-elt adjectives))
         (noun (seq-random-elt nouns))
         (suffix (cl-random token-range)))
    (if prefix
        (format "%s-%s-%s-%d" prefix adjective noun suffix)
      (format "%s-%s-%d" adjective noun suffix))))

(defun haikens (limit token-range prefix)
  "Generate LIMIT random names."
  (-map (lambda (n) (haikunate token-range prefix)) (number-sequence 1 limit)))

(defun create-java-project (artifact-id)
  (interactive
   (list
    (ivy-read "Project name: "
              (haikens 4 100 project-prefix))))
  (let* ((default-directory project-dir)
         (arch-version "1.4")
         (group-id "com.example")
         (app-version "0.1")
         (app-dir (concat project-dir "/" artifact-id))
         (app-projectile-path (concat app-dir "/.projectile"))
         (cmd "mvn")
         (args (list "archetype:generate"
                     "-DarchetypeGroupId=org.apache.maven.archetypes"
                     "-DarchetypeArtifactId=maven-archetype-simple"
                     (format "-DarchetypeVersion=%s" arch-version)
                     (format "-DgroupId=%s" group-id)
                     (format "-DartifactId=%s" artifact-id)
                     (format "-Dversion=%s" app-version))))
    (if (executable-find "mvn")
        (progn (apply #'doom-call-process cmd args)
               (f-touch app-projectile-path)
               (projectile-discover-projects-in-search-path)
               (when (fboundp 'lsp-workspace-folders-add)
                 (lsp-workspace-folders-add app-dir))
               (message "created project %s" artifact-id))
      (user-error "executable %s not found" cmd))))

(defun create-scala-project (name)
  (interactive
   (list
    (ivy-read "Project name: "
              (haikens 4 100 project-prefix))))
  (let* ((default-directory project-dir)
         (app-dir (concat project-dir "/" name))
         (app-projectile-path (concat app-dir "/.projectile"))
         (cmd "sbt")
         (args (list "new"
                     "scala/scala-seed.g8"
                     (format "--name=%s" name))))
    (if (executable-find cmd)
        (progn (apply #'doom-call-process cmd args)
               (f-touch app-projectile-path)
               (projectile-discover-projects-in-search-path)
               (message "created project %s" name))
      (user-error "executable %s not found" cmd))))

(defun create-clojure-project (name)
  (interactive
   (list
    (ivy-read "Project name: "
              (haikens 4 100 project-prefix))))
  (let* ((default-directory project-dir)
         (app-dir (concat project-dir "/" name))
         (app-projectile-path (concat app-dir "/.projectile"))
         (cmd "lein")
         (args (list "new"
                     "app"
                     name)))
    (if (executable-find cmd)
        (progn (apply #'doom-call-process cmd args)
               (f-touch app-projectile-path)
               (projectile-discover-projects-in-search-path)
               (message "created project %s" name))
      (user-error "executable %s not found" cmd))))

(defun delete-project (project-path)
  "Delete mvn project.
Delete mvn project at PROJECT-PATH by removing project from lsp workspaces,
removing project from projectile and deleting project folders.
Beware using this command given that it's destructive and non reversible."
  (interactive
   (list
    (ivy-read "Project name: "
              (if counsel-projectile-remove-current-project
                  (projectile-relevant-known-projects)
                projectile-known-projects))))
  (let* ((project-name (car (last (s-split "/" (string-trim project-path "/" "/"))))))
    (progn (when (fboundp 'lsp-workspace-folders-remove)
             (lsp-workspace-folders-remove project-path))
           (when (+workspace-exists-p project-name)
             (+workspace-delete project-name))
           (projectile-remove-known-project (concat (string-trim-right project-path "/") "/"))
           (f-delete project-path t)
           (message "deleted project %s" project-path))))

(defun projects-cleanup ()
  "Delete all test projects."
  (interactive)
  (let* ((projects (f-directories project-dir))
         (matches  (-filter (lambda (project) (s-contains? project-prefix project)) projects)))
    (seq-do #'delete-project matches)))

(map! :leader
      (:prefix-map ("o" . "open")
       (:prefix ("s" . "spotify")
        (:prefix ("p" . "projects")
         :desc "create java project" "j" #'create-java-project
         :desc "create scala project" "s" #'create-scala-project
         :desc "create clojure project" "c" #'create-clojure-project
         :desc "delete project" "d" #'delete-project
         :desc "delete all test projects" "D" #'projects-cleanup))))

;; (add-to-list 'load-path mu4e-path)
(set-email-account! "Spotify"
  '((mu4e-sent-folder       . "/spotify/sent")
    (mu4e-drafts-folder     . "/spotify/drafts")
    (mu4e-trash-folder      . "/spotify/trash")
    (mu4e-refile-folder     . "/spotify/All Mail")
    (smtpmail-smtp-user     . "zangao@spotify")
    (smtpmail-smtp-server   . "smtp.gmail.com")
    (smtpmail-smtp-service  . 465)
    (smtpmail-stream-type   . ssl)
    (user-mail-address      . "zangao@spotify.com")    ;; only needed for mu < 1.4
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

;; this won't work temporarily for protonmail as certificates are being moved to /etc/ssl/certs
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles "~/.config/certificates/protonmail.crt"))

;; (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart)

(setq mu4e-bookmarks
      '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
        (:name "Today's messages" :query "date:today..now" :key 116)
        (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
        (:name "Messages with images" :query "mime:image/*" :key 112)
        (:name "Fragomen" :query "fragomen" :hide-unread t :key 102)))

(defun beno--capture-mail-follow-up (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "mf"))

(defun beno--capture-mail-read-later (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "mr"))

;; store query link is convenient for capturing search query for use in org mail
(defun beno--store-mu4e-query-link ()
  (interactive)
  (let ((mu4e-org-link-query-in-headers-mode t))
    (call-interactively 'org-store-link)))

(after! mu4e
  (add-to-list 'mu4e-headers-actions '("follow up" . beno--capture-mail-follow-up) t)
  (add-to-list 'mu4e-view-actions '("follow up" . beno--capture-mail-follow-up) t)
  (add-to-list 'mu4e-headers-actions '("read later" . beno--capture-mail-read-later) t)
  (add-to-list 'mu4e-view-actions '("read later" . beno--capture-mail-read-later) t))

(after! dired-single
  (map! :after dired-single
        :map dired-mode-map
        :n "h" 'dired-single-up-directory
        :n "l" 'dired-single-buffer))

(defun beno--valid-json? (maybe-json)
  "Validate MAYBE-JSON is json."
  (condition-case nil
      (progn
        (json-read-from-string maybe-json)
        t)
    (error nil)))

;; TODO: refactor these variables in a cons e.g (cons beg end)
(setq beno--eshell-output-beg nil)
(setq beno--eshell-output-end nil)

(defun beno--eshell-json-print ()
  (let* ((start (marker-position eshell-last-output-start))
         (end (marker-position eshell-last-output-end))
         (partial-output (buffer-substring start end)))
    (if (s-matches? eshell-prompt-regexp partial-output)
        (condition-case nil
            (progn
              (when (and beno--eshell-output-beg
                         beno--eshell-output-end
                         (beno--valid-json? (buffer-substring beno--eshell-output-beg
                                                              beno--eshell-output-end)))
                (json-pretty-print beno--eshell-output-beg beno--eshell-output-end))
              (setq beno--eshell-output-beg nil)
              (setq beno--eshell-output-end nil))
          (error (progn
                   (setq beno--eshell-output-beg nil)
                   (setq beno--eshell-output-end nil))))
      (progn
        (unless beno--eshell-output-beg
          (setq beno--eshell-output-beg (marker-position eshell-last-output-start)))
        (setq beno--eshell-output-end (marker-position eshell-last-output-end))))))

(with-eval-after-load 'eshell
  (add-to-list 'eshell-output-filter-functions
               #'beno--eshell-json-print))

(defun project-tests (project-path)
  "Extract java TESTS at PROJECT-PATH."
  (-filter (lambda (filename) (or (s-contains? "IT.java" filename)
                             (s-contains? "Test.java" filename)))
           (-map (lambda (filepath) (-last-item  (s-split "/" filepath)))
                 (f-files project-path nil t))))

(defun test-to-run (test-name)
  "Prompt for TEST-NAME to run."
  (interactive
   (list  (ivy-read "Test to run: "
                    (project-tests default-directory))))
  (format "mvn clean -Dtest=%s test" test-name))

(defun package-no-test ()
  "Command to package application without running tests"
  (format "mvn -Dmaven.test.skip=true clean package"))

(defun eshell/pkg ()
  "Package java application."
  (insert (package-no-test)))

(defun eshell/gst (&rest args)
  "Quickly jumps to magit-status."
    (magit-status (pop args) nil)
    (eshell/echo))

(defun eshell/test ()
  "Run java tests."
  (eshell/cd-to-project)
  (+eshell/goto-end-of-prompt)
  (insert (call-interactively 'test-to-run)))

;; (require 'load-nano)
