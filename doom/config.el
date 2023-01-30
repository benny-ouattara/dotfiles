(require 'f)
(require 's)
(require 'dash)

(setq rfc-mode-directory (expand-file-name "~/rfc/"))

(setq
 user-full-name "Ben O"
 user-mail-address "benny.ouattara@gmail.com"
 home-dir (getenv "HOME"))

(setq
 project-dir "~/Code"
 project-prefix "kata"
 skeletor-project-directory project-dir
 skeletor-user-directory (concat doom-private-dir "templates")
 skeletor-completing-read-function 'ivy-read)

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

(setq
 org-pomodoro-length 45
 org-pomodoro-short-break-length 15)

(if (equal "zangao" (user-login-name))
    (setq
     ;; doom-font (font-spec :family "monaco" :size 15 :weight 'normal)
     ;; doom-font (font-spec :family "JetBrains Mono" :size 19 :weight 'normal :width 'normal)
     ;; doom-variable-pitch-font (font-spec :family "Avenir Next" :size 21)
     doom-font (font-spec :family "Iosevka" :size 21 :weight 'normal)
     doom-big-font (font-spec :family "Iosevka" :size 27)
     doom-theme 'modus-operandi)
  (setq
   ;; doom-font (font-spec :family "monaco" :size 15 :weight 'normal)
   ;; doom-font (font-spec :family "JetBrains Mono" :size 19 :weight 'normal :width 'normal)
   ;; doom-variable-pitch-font (font-spec :family "Avenir Next" :size 21)
   doom-font (font-spec :family "Iosevka" :size 19 :weight 'normal)
   doom-big-font (font-spec :family "Iosevka" :size 25)
   doom-theme 'doom-palenight))

(setq
 mac-command-modifier 'meta
 ns-command-modifier 'meta
 confirm-kill-emacs nil
 evil-insert-state-cursor 'hbar
 evil-move-cursor-back nil
 display-line-numbers-type 'relative
 fancy-splash-image (expand-file-name "splash.png" doom-private-dir))

(menu-bar-mode -1)
(rainbow-mode)

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;; (defun beno--auto-theme (appearance)
;;   "Load theme, taking current system APPEARANCE into consideration."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (pcase appearance
;;     ('light (load-theme 'doom-one-light t))
;;     ('dark (load-theme 'doom-snazzy t))))

(add-hook 'ns-system-appearance-change-functions #'beno--auto-theme)

(when (not (display-graphic-p))
  (progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (mapc #'disable-theme custom-enabled-themes)
    (setq doom-theme 'doom-acario-light
          evil-emacs-state-cursor '("red" box)
          evil-normal-state-cursor '("black" box)
          evil-visual-state-cursor '("black" box)
          evil-insert-state-cursor '("red" bar)
          evil-motion-state-cursor '("gray" box))))

(setq
 ;; org-ellipsis " ▾ "
 ;; org-ellipsis " ⤵ "
 ;; org-ellipsis " ↓ "
 ;; org-ellipsis " ∵ "
 ;; org-ellipsis " ⌄ "
 ;; org-ellipsis " ⁂ "
 org-startup-folded 'content
 org-auto-align-tags nil
 org-roam-v2-ack t
 +org-roam-open-buffer-on-find-file nil
 sync-dir "~/Sync/"
 org-directory (concat sync-dir "org")
 org-spotify-directory (concat org-directory "/spotify")
 org-mail-directory (concat org-directory "/mail.org")
 org-mime-export-options '(:section-numbers nil
                           :with-author nil
                           :with-toc nil)
 org-agenda-files (ignore-errors (directory-files org-directory t "\\.org$" t))
 ;; org-ellipsis " ≡ "
 org-ellipsis " ▾"
 org-hide-emphasis-markers t
 org-tags-column -80
 org-log-done 'time
 org-refile-targets (quote ((nil :maxlevel . 3)))
 +org-capture-todo-file "tasks.org")

(after! org
  (pushnew! org-capture-templates
            '("m" "Email workflow")
            '("mf" "Follow up" entry (file+olp org-mail-directory "Follow up")
              "* TODO follow up with %:fromname on %a\n\n%i"
              :immediate-finish t)
            '("mr" "Read later" entry (file+olp org-mail-directory "Read later")
              "* TODO read %:subject\n%a\n\n%i"
              :immediate-finish t)))

(setq-hook! org-mode
  prettify-symbols-alist '(("#+end_quote" . "”")
                           ("#+END_QUOTE" . "”")
                           ("#+begin_quote" . "“")
                           ("#+BEGIN_QUOTE" . "“")
                           ("#+end_src" . "«")
                           ("#+END_SRC" . "«")
                           ("#+begin_src" . "»")
                           ("#+BEGIN_SRC" . "»")
                           ("#+name:" . "»")
                           ("#+NAME:" . "»")))

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(setq org-roam-dailies-capture-templates '(("d" "default" plain
                                            "* %?"
                                            :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
                                            :unnarrowed t)
                                           ("a" "daily plan" plain
                                            (file "~/Code/dotfiles/doom/snippets/org-roam/daily.org")
                                            :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
(setq org-roam-capture-templates '(("d" "default" plain
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/default.org")
                                    :target (file+head  "%<%Y%m%d%H%M%S>-${slug}.org"  "#+title: ${title}\n#+date: %U\n")
                                    :unnarrowed t)
                                   ("l" "programming language" plain
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/programming.org")
                                    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: programming\n")
                                    :unnarrowed t)
                                   ("b" "book notes" plain
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/book.org")
                                    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: book\n")
                                    :unnarrowed t)
                                   ("p" "project" plain
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/project.org")
                                    :target (file+head  "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: project\n")
                                    :unnarrowed t)
                                   ("c" "code" plain
                                    (file "~/Code/dotfiles/doom/snippets/org-roam/code.org")
                                    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"  "#+title: ${title}\n#+date: %U\n#+filetags: interview\n")
                                    :unnarrowed t)))

(defun beno/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Completed Tasks" today-file nil pos)))))

(after! org
  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (beno/org-roam-copy-todo-to-today)))))

(setq org-fold-core-style 'overlays)

(after! org-journal
  (map! :leader :desc "Open current journal" "j" #'org-journal-open-current-journal-file))

(setq
 tramp-histfile-override "/dev/null")

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

(map! :leader
      :desc "close current window"
      "0" #'evil-quit)

(map! :leader
      :desc "close other window"
      "9" #'delete-other-windows)

(map! :leader
      :desc "split with eshell"
      ">" #'beno--eshell-toggle-right)

(map! :desc "fuzzy search visible buffer"
      :leader
      "a" #'evil-avy-goto-char-2)

(map! :desc "line in visible buffer"
      :leader
      "A" #'avy-goto-line)

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

(map! :leader
      :desc "calendar"
      "o c" #'cfw:open-calendar-buffer)

(map! :leader
      (:prefix-map ("o" . "open")
       (:prefix ("s" . "spotify")
        (:prefix ("p" . "projects")
         :desc "create java project" "j" #'create-java-project
         :desc "create scala project" "s" #'create-scala-project
         :desc "create clojure project" "c" #'create-clojure-project
         :desc "create common lisp project" "l" #'create-common-lisp-project
         :desc "delete project" "d" #'delete-project
         :desc "delete all test projects" "D" #'projects-cleanup))))

(map! :after cc-mode
      :map java-mode-map
      :localleader
      (:prefix ("c" . "Compile")
       :desc "Compile mvn project"  "c" (cmd! (beno--run-mvn-command "clean compile"))
       :desc "Verify mvn project"   "v" (cmd! (beno--run-mvn-command "clean verify"))
       :desc "Package mvn project"  "p" (cmd! (beno--run-mvn-command "clean package"))
       :desc "Package mvn project - skip tests"  "P" (cmd! (beno--run-mvn-command "-Dmaven.test.skip=true clean package"))
       :desc "Test mvn project"  "t" (cmd! (beno--run-mvn-command "clean test"))
       :desc "Integration test mvn project"  "i" (cmd! (beno--run-mvn-command "-Dtest=SomeNonExistingTestClass -DfailIfNoTests=false integration-test"))
       :desc "Run test"  "T" (cmd! (beno--run-mvn-command (call-interactively #'beno--mvn-test-to-run)))))

(setq
 lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")

;; setup lsp server for eglot
;; eglot doesn't recognize ~ for user home directory
(setq lsp-jar (concat home-dir "/.emacs.d/.local/etc/lsp/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar"))

(defun set-lsp-jar ()
  (setenv "CLASSPATH" lsp-jar))

(add-hook 'java-mode-hook #'set-lsp-jar)

(setq  lsp-java-vmargs
       (list
        "-noverify"
        "-Xmx2G"
        "-Xms100m"
        "-Dsun.zip.disableMemoryMapping=true"
        "-XX:+UseG1GC"
        "-XX:+UseStringDeduplication"
        ))

;; breadcrumb is a nice feature to know about, not using it now
;; (after! lsp-mode
;;   (lsp-headerline-breadcrumb-mode))

;; makes lsp-mode a little more bearable: hide all the UI noise
(after! (lsp-mode lsp-ui)
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-doc-enable nil)
  (lsp-ui-doc-mode -1))

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

(defun create-common-lisp-project (name)
  (interactive
   (list
    (ivy-read "Project name: "
              (haikens 4 100 project-prefix))))
  (let* ((default-directory project-dir)
         (app-dir (concat project-dir "/" name))
         (app-projectile-path (concat app-dir "/.projectile"))
         (cmd "sbcl")
         (args (list "--non-interactive"
                     "--eval" "(ql:quickload :cl-project)"
                     "--eval" (format "(cl-project:make-project #p\"%s\" :author %s :email %s :depends-on '())" name "\"Ben O.\"" "\"benny.ouattara@gmail.com\""))))
    (unless (executable-find cmd)
      (user-error "executable %s not found" cmd))
    (let* ((result (apply #'doom-call-process cmd args))
           (status (car result)))
      (if (equal status 0)
          (progn
            (f-touch app-projectile-path)
            (projectile-discover-projects-in-search-path)
            (message "created project %s" name))
        (message (format "failed to create project. exit code %d" status))))))

(defun delete-project (project-path)
  "Delete kata project.
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

(if (not (equal "ben" (user-login-name)))
    (progn (setq
            mu-root (s-chop-suffixes '("/mu" "/bin") (file-truename  (executable-find "mu")))
            mu4e-path (concat mu-root "/share/emacs/site-lisp/mu4e")
            mu4e-update-interval 60)
           (add-to-list 'load-path mu4e-path)))

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

;; this won't work temporarily for protonmail as certificates are being moved to /etc/ssl/certs
(after! gnutls
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

(after! eshell
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
  (format "clear && mvn clean -Dtest=%s -DfailIfNoTests=false test" test-name))

(defun package-no-test ()
  "Command to package application without running tests"
  (format "clear && mvn -Dmaven.test.skip=true clean package"))

(defun package-verify ()
  "Command to verify application"
  (format "clear && mvn clean verify"))

(defun package-compile ()
  "Command to verify application"
  (format "clear && mvn clean compile"))

(defun eshell/pkg ()
  "Package java application."
  (insert (package-no-test)))

(defun eshell/compile ()
  "Compile java application."
  (insert (package-compile)))

(defun eshell/verify ()
  "Verify java application."
  (insert (package-verify)))

(defun eshell/gst (&rest args)
  "Quickly jumps to magit-status."
  (magit-status (pop args) nil)
  (eshell/echo))

(defun eshell/test ()
  "Run java tests."
  (eshell/cd-to-project)
  (+eshell/goto-end-of-prompt)
  (insert (call-interactively 'test-to-run)))

(setf +main-eshell-popup+ "*doom:eshell-popup:main*")

(defun beno--eshell-toggle-right (arg &optional command)
  "Toggle eshell popup window to the right"
  (interactive "P")
  (let ((eshell-buffer
         (get-buffer-create +main-eshell-popup+))
        confirm-kill-processes
        current-prefix-arg)
    (when arg
      (when-let (win (get-buffer-window eshell-buffer))
        (delete-window win))
      (when (buffer-live-p eshell-buffer)
        (with-current-buffer eshell-buffer
          (fundamental-mode)
          (erase-buffer))))
    (if-let (win (get-buffer-window eshell-buffer))
        (let (confirm-kill-processes)
          (delete-window win)
          (ignore-errors (kill-buffer eshell-buffer)))
      (with-current-buffer eshell-buffer
        (doom-mark-buffer-as-real-h)
        (if (eq major-mode 'eshell-mode)
            (run-hooks 'eshell-mode-hook)
          (eshell-mode))
        (when command
          (+eshell-run-command command eshell-buffer)))
      (pop-to-buffer eshell-buffer))))

(defun beno--eshell-split-right ()
  "Create a new eshell window 2/3 to the right of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p))
         (+eshell-enable-new-shell-on-split
          (or +eshell-enable-new-shell-on-split (frame-parameter nil 'saved-wconf))))
    (select-window (split-window-horizontally (* 2 (/ (window-total-width) 3))))
    (+eshell--bury-buffer dedicated-p)))

;; (require 'load-nano)

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(after! avy
  (setf (alist-get ?D avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?Z  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?H avy-dispatch-alist) 'avy-action-helpful
        (alist-get ?\; avy-dispatch-alist) 'avy-action-embark))

(when (> (display-pixel-width) 3000)
  (set-popup-rule! "*cp:eshell*" :size 0.40 :height 0.25 :slot 90 :select t :quit nil :ttl t :side 'right)
  (set-popup-rule! "err.txt" :size 0.40 :height 0.25 :slot 100 :select t :quit nil :ttl t :side 'right :modeline t)
  (set-popup-rule! "output.txt" :size 0.40 :height 0.25 :slot 110 :select t :quit nil :ttl t :side 'right :modeline t)
  (set-popup-rule! "input.txt" :size 0.40 :height 0.25 :slot 120 :select t :quit nil :ttl t :side 'right :modeline t))

(cl-defstruct solution-info
  (ext nil :read-only t)
  (dir nil :read-only t)
  (template nil :read-only t))

(defun initialize-lang-info ()
  (let* ((lang-info (make-hash-table))
         (python-ext "py")
         (python-dir (concat project-dir "/algo-python"))
         (python-template "import sys
sys.stdin = open(\"input.txt\", \"r\")
sys.stdout = open(\"output.txt\", \"w\")
sys.stderr = open(\"err.txt\", \"w\")")
         (ruby-ext "rb")
         (ruby-dir (concat project-dir "/algo-ruby"))
         (ruby-template "$stdin = File.open(\"input.txt\", \"r\")
$stdout = File.open(\"output.txt\", \"w\")
$stderr = File.open(\"err.txt\", \"w\")")
         (scala-ext "scala")
         (scala-dir (concat project-dir "/algo-scala"))
         (scala-template "")
         (python-info (make-solution-info :ext python-ext :dir python-dir :template python-template))
         (ruby-info (make-solution-info :ext ruby-ext :dir ruby-dir :template ruby-template))
         (scala-info (make-solution-info :ext scala-ext :dir scala-dir :template scala-template)))
    (puthash :py python-info lang-info)
    (puthash :scala scala-info lang-info)
    (puthash :rb ruby-info lang-info)
    lang-info))

(defun cp-solve (language problem-name)
  (interactive "slang: \nsproblem name: \n")
  (let* ((info-table (initialize-lang-info))
         (lang (doom-keyword-intern language))
         (lang-info (gethash lang info-table))
         (solution-directory (solution-info-dir lang-info))
         (ext (solution-info-ext lang-info))
         (lang-template (solution-info-template lang-info))
         (solution-directory-path (concat solution-directory "/" problem-name))
         (solution-file-path (concat solution-directory-path "/" "sol." ext))
         (input-file-path (concat solution-directory-path "/" "input.txt"))
         (output-file-path (concat solution-directory-path "/" "output.txt"))
         (error-file-path (concat solution-directory-path "/" "err.txt"))
         (file-paths (list input-file-path output-file-path error-file-path solution-file-path))
         (height (/ (window-total-height) 4)))
    (make-directory solution-directory-path 'parents)
    (-map #'f-touch file-paths)
    (with-current-buffer (find-file solution-file-path)
      (when (= (buffer-size) 0) (insert lang-template))
      (save-buffer))
    (let ((eshell-buffer-name "*cp:eshell*"))
      (eshell))
    (display-buffer (find-file-noselect error-file-path))
    (display-buffer (find-file-noselect output-file-path))
    (display-buffer (find-file-noselect input-file-path))))

(defun save-all-buffers ()
  (save-some-buffers t))

(add-to-list 'doom-switch-buffer-hook #'save-all-buffers)
(add-to-list 'doom-switch-window-hook #'save-all-buffers)
(add-to-list 'doom-switch-frame-hook #'save-all-buffers)

(custom-set-faces!
  '(wgrep-face :background "#aceaac" :foreground "#004c00"))

(setq
 secrets-dir (concat sync-dir "secrets/")
 zangao-secrets (concat secrets-dir "zangao/authinfo.gpg")
 bouattara-secrets (concat secrets-dir "bouattara/authinfo.gpg")
 benny-secrets (concat secrets-dir "benny/authinfo.gpg"))

(pcase (user-login-name)
  ("zangao" (pushnew! auth-sources zangao-secrets))
  ("bouattara" (pushnew! auth-sources bouattara-secrets))
  ("benny" (pushnew! auth-sources benny-secrets)))

(defun beno--read-db-password (db)
  (if-let ((result (auth-source-search :database db)))
      (funcall (plist-get  (car result) :secret))))

(defun beno--sql-authenticator (wallet product user server database port)
  (beno--read-db-password database))

(after! sql
  (setq
   setcheckerpwd (beno--read-db-password "setchecker_runs")
   sql-password-search-wallet-function #'beno--sql-authenticator
   sql-password-wallet zangao-secrets
   sql-connection-alist `(("setchecker-cloudsql-connection"
                           (sql-product 'postgres)
                           (sql-user "postgres")
                           ;; password reading is done through pgpass since psql cli does't support password passing
                           ;; this line just makes sure that sql.el doesn't ask us for the a dummy password
                           (sql-password ,setcheckerpwd)
                           (sql-database "setchecker_runs")
                           (sql-server "localhost")
                           (sql-port 5432)))
   sql-postgres-login-params '(user password database server)))

(defun beno--mvn-root-dir ()
  (or (locate-dominating-file buffer-file-name ".git")
      (projectile-project-root)))

(defun beno--run-mvn-command (command)
  (interactive "sCommand: ")
  (let ((default-directory (beno--mvn-root-dir))
        (compilation-read-command nil)
        (compile-command (format "sh mvn %s" command)))
    (call-interactively #'compile)))

(defun beno--mvn-project-tests (project-path)
  "Extract java TESTS at PROJECT-PATH."
  (-filter (lambda (filename) (or (s-contains? "IT.java" filename)
                                  (s-contains? "Test.java" filename)))
           (-map (lambda (filepath) (-last-item  (s-split "/" filepath)))
                 (f-files project-path nil t))))

(defun beno--mvn-test-to-run (test-name)
  "Prompt for TEST-NAME to run."
  (interactive
   (list  (ivy-read "Test to run: "
                    (beno--mvn-project-tests (beno--mvn-root-dir)))))
  (format "clean -DfailIfNoTests=false -Dtest=%s test" test-name))

(setq
 projectile-project-search-path '("~/Code/" "~/common-lisp" "~/Code/archives/Code"))

(after! projectile
  (setq projectile-project-root-files-bottom-up
        (remove ".project" projectile-project-root-files-bottom-up)))

(after! lsp-java
  (setq  projectile-project-test-cmd "sh mvn clean test"
         projectile-project-compilation-cmd "sh mvn clean compile"
         projectile-project-install-cmd "sh mvn clean install"
         projectile-project-package-cmd "sh mvn clean verify"
         projectile-project-run-cmd "docker run --rm --dns 1.1.1.1 -p 8080:8080 -p 5990:5990 -p 5700:5700 -e SPOTIFY_DOMAIN=gew1.spotify.net -e SPOTIFY_POD=gew1 $(jq -r '.image' target/jib-image.json)")
  (map! :leader
        :desc "Verify project"
        :n "p P"
        'projectile-package-project))

;; (set-popup-rule! "^2022" :size 0.40 :vslot -4 :select t :ttl t :quit nil :side 'right)
;; (set-popup-rule! "^2021" :size 0.40 :vslot -4 :select t :ttl t :quit nil :side 'right)
;; (set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
;; (set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)
;; (set-popup-rule! "org$" :size 0.33 :vslot -4 :select t :ttl t :quit nil :side 'right)
(if (> (display-pixel-width) 3000)
    ;; large display
    (progn
      (set-popup-rule! +main-eshell-popup+ :size 0.33 :vslot -4 :select t :quit nil :ttl t :side 'right)
      (set-popup-rule! "*SQL:" :size 0.33 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
      (set-popup-rule! "^\\*compilation.*" :size 0.33 :vslot -4 :select t :quit nil :ttl t :side 'right)
      (set-popup-rule! "^\\*Org Agenda\\*" :size 0.33 :vslot -4 :select t :quit nil :ttl t :side 'right)
      (set-popup-rule! "[0-9]+-[0-9]+-[0-9]+.org" :size 0.33 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
      (set-popup-rule! "journal.org" :size 0.25 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
      (set-popup-rule! "^[0-9]\\{8\\}$" :size 0.33 :vslot -4 :select t :quit 'other :ttl nil :side 'right :autosave t)
      (set-popup-rule! "*kubel" :size 0.50 :vslot -4 :select t :quit nil :ttl t :side 'right))
  ;; small display
  (progn
    ;; (set-popup-rule! +main-eshell-popup+ :size 0.25 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
    ;; (set-popup-rule! "^\\*compilation.*" :size 0.25 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
    (set-popup-rule! "*SQL:" :size 0.25 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
    ;; (set-popup-rule! "^\\*Org Agenda\\*" :size 0.25 :vslot -4 :select t :quit nil :ttl t :side 'right)
    ;; (set-popup-rule! "[0-9]+-[0-9]+-[0-9]+.org" :size 0.25 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
    ;; (set-popup-rule! "journal.org" :size 0.25 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
    ;; (set-popup-rule! "^[0-9]\\{8\\}$" :size 0.25 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
    ))

;; (vertico-posframe-mode 1)
;; (setq vertico-posframe-parameters
;;       '((left-fringe . 8)
;;         (right-fringe . 8)))

(add-to-list 'default-frame-alist '(undecorated . t))

(add-hook 'elfeed-search-mode-hook #'elfeed-update)

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "/home/ben/Code/guix-turtle")
  ;; (add-to-list 'geiser-guile-load-path "/home/ben/Code/guix")
  )

;; (use-package! info-colors
;;   :after info
;;   :commands (info-colors-fontify-node)
;;   :hook (Info-selection . info-colors-fontify-node))

(after! notmuch
  (setq +notmuch-sync-backend 'mbsync)
  ;; (after! notmuch
  ;;   (setq notmuch-show-log nil
  ;;         notmuch-hello-sections `(notmuch-hello-insert-saved-searches
  ;;                                  notmuch-hello-insert-alltags)
  ;;         ;; To hide headers while composing an email
  ;;         notmuch-message-headers-visible nil))
  (setq notmuch-saved-searches '((:name "inbox" :query "tag:inbox not tag:trash" :key "i")
                                 ;; (:name "flagged" :query "tag:flagged" :key "f")
                                 ;; (:name "sent" :query "tag:sent" :key "s")
                                 ;; (:name "drafts" :query "tag:draft" :key "d")
                                 (:name "spotify" :query "tag:spotify" :key "s")
                                 (:name "gmail" :query "tag:gmail" :key "g")
                                 (:name "protonmail" :query "tag:protonmail" :key "p")
                                 (:name "spotify-unread" :query "tag:spotify and tag:unread" :key "S")
                                 (:name "gmail-unread" :query "tag:gmail and tag:unread" :key "G")
                                 (:name "protonmail-unread" :query "tag:protonmail and tag:unread" :key "P")))

  (set-popup-rule! "^\\*notmuch-hello" :ignore t)
  (set-popup-rule! "^\\*notmuch-saved" :ignore t)

  (map! :localleader
        :map (notmuch-hello-mode-map notmuch-search-mode-map notmuch-tree-mode-map notmuch-show-mode-map)
        :desc "Compose email"   "c" #'+notmuch/compose
        :desc "Sync email"      "u" #'+notmuch/update
        :desc "Quit notmuch"    "q" #'+notmuch/quit
        :desc "Mark as read"    "r" #'notmuch-show-mark-read
        :map notmuch-search-mode-map
        :desc "Mark as deleted" "d" #'+notmuch/search-delete
        :desc "Mark as spam"    "s" #'+notmuch/search-spam
        :map notmuch-tree-mode-map
        :desc "Mark as deleted" "d" #'+notmuch/tree-delete
        :desc "Mark as spam"    "s" #'+notmuch/tree-spam))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-loaded)

;; (global-subword-mode 1)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(unless (string= "" (shell-command-to-string "pgrep stumpwm"))
  (set-frame-parameter (selected-frame) 'alpha-background 90)
  (add-to-list 'default-frame-alist '(alpha-background . 90)))

(global-org-modern-mode)

(after! modus-themes
  (setq modus-themes-syntax '(faint alt-syntax green-strings yellow-comments))
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-variable-pitch-ui nil
        modus-themes-mixed-fonts nil)

  (setq modus-themes-prompts '(bold))
  (setq modus-themes-completions nil)
  (setq modus-themes-org-blocks 'gray-background))

# Configuration for Alacritty, the GPU enhanced terminal emulator.

# Any items in the `env` entry below will be added as
# environment variables. Some entries may override variables
# set by alacritty itself.
#env:
  # TERM variable
  #
  # This value is used to set the `$TERM` environment variable for
  # each instance of Alacritty. If it is not present, alacritty will
  # check the local terminfo database and use `alacritty` if it is
  # available, otherwise `xterm-256color` is used.
  #TERM: alacritty

window:
  # Window dimensions (changes require restart)
  #
  # Specified in number of columns/lines, not pixels.
  # If both are `0`, this setting is ignored.
  #dimensions:
  #  columns: 0
  #  lines: 0

  # Window position (changes require restart)
  #
  # Specified in number of pixels.
  # If the position is not set, the window manager will handle the placement.
  #position:
  #  x: 0
  #  y: 0

  # Window padding (changes require restart)
  #
  # Blank space added around the window in pixels. This padding is scaled
  # by DPI and the specified value is always added at both opposing sides.
  #padding:
  #  x: 0
  #  y: 0

  # Spread additional padding evenly around the terminal content.
  #dynamic_padding: false

  # Window decorations
  #
  # Values for `decorations`:
  #     - full: Borders and title bar
  #     - none: Neither borders nor title bar
  #
  # Values for `decorations` (macOS only):
  #     - transparent: Title bar, transparent background and title bar buttons
  #     - buttonless: Title bar, transparent background, but no title bar buttons
  decorations: buttonless

  # Startup Mode (changes require restart)
  #
  # Values for `startup_mode`:
  #   - Windowed
  #   - Maximized
  #   - Fullscreen
  #
  # Values for `startup_mode` (macOS only):
  #   - SimpleFullscreen
  #startup_mode: Windowed

  # Window title
  #title: Alacritty

  # Window class (Linux/BSD only):
  #class:
    # Application instance name
    #instance: Alacritty
    # General application class
    #general: Alacritty

  # GTK theme variant (Linux/BSD only)
  #
  # Override the variant of the GTK theme. Commonly supported values are `dark` and `light`.
  # Set this to `None` to use the default theme variant.
  #gtk_theme_variant: None

scrolling:
  # Maximum number of lines in the scrollback buffer.
  # Specifying '0' will disable scrolling.
  history: 10000

  # Number of lines the viewport will move for every line scrolled when
  # scrollback is enabled (history > 0).
  #multiplier: 3

# Font configuration
font:
  # Normal (roman) font face
  normal:
    # Font family
    #
    # Default:
    #   - (macOS) Menlo
    #   - (Linux/BSD) monospace
    #   - (Windows) Consolas
    family: Monaco

    # The `style` can be specified to pick a specific face.
    #style: Regular

  # Bold font face
  #bold:
    # Font family
    #
    # If the bold family is not specified, it will fall back to the
    # value specified for the normal font.
    #family: monospace

    # The `style` can be specified to pick a specific face.
    #style: Bold

  # Italic font face
  #italic:
    # Font family
    #
    # If the italic family is not specified, it will fall back to the
    # value specified for the normal font.
    #family: monospace

    # The `style` can be specified to pick a specific face.
    #style: Italic

  # Bold italic font face
  #bold_italic:
    # Font family
    #
    # If the bold italic family is not specified, it will fall back to the
    # value specified for the normal font.
    #family: monospace

    # The `style` can be specified to pick a specific face.
    #style: Bold Italic

  # Point size
  size: 17.0

  # Offset is the extra space around each character. `offset.y` can be thought of
  # as modifying the line spacing, and `offset.x` as modifying the letter spacing.
  #offset:
  #  x: 0
  #  y: 0

  # Glyph offset determines the locations of the glyphs within their cells with
  # the default being at the bottom. Increasing `x` moves the glyph to the right,
  # increasing `y` moves the glyph upwards.
  #glyph_offset:
  #  x: 0
  #  y: 0

  # Thin stroke font rendering (macOS only)
  #
  # Thin strokes are suitable for retina displays, but for non-retina screens
  # it is recommended to set `use_thin_strokes` to `false`
  #
  # macOS >= 10.14.x:
  #
  # If the font quality on non-retina display looks bad then set
  # `use_thin_strokes` to `true` and enable font smoothing by running the
  # following command:
  #   `defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO`
  #
  # This is a global setting and will require a log out or restart to take
  # effect.
  #use_thin_strokes: true

# If `true`, bold text is drawn using the bright color variants.
#draw_bold_text_with_bright_colors: false

# Hyper color theme
colors:
# Default colors
  primary:
    background: '#000000'
    foreground: '#ffffff'
  cursor:
    text: '#F81CE5'
    cursor: '#ffffff'

  # Normal colors
  normal:
    black:   '#000000'
    red:     '#fe0100'
    green:   '#33ff00'
    yellow:  '#feff00'
    blue:    '#0066ff'
    magenta: '#cc00ff'
    cyan:    '#00ffff'
    white:   '#d0d0d0'

  # Bright colors
  bright:
    black:   '#808080'
    red:     '#fe0100'
    green:   '#33ff00'
    yellow:  '#feff00'
    blue:    '#0066ff'
    magenta: '#cc00ff'
    cyan:    '#00ffff'
    white:   '#FFFFFF'

######## Rigel color theme ##############

# colors:
#   # Default colors
#   primary:
#     background: '0x002635'
#     foreground: '0xe6e6dc'

#   # Normal colors
#   normal:
#     black:   '0x00384d'
#     red:     '0xc43061'
#     green:   '0x7fc06e'
#     yellow:  '0xf08e48'
#     blue:    '0x1c8db2'
#     magenta: '0xc694ff'
#     cyan:    '0x00cccc'
#     white:   '0x77929e'

#   # Bright colors
#   bright:
#     black:   '0x517f8d'
#     red:     '0xff5a67'
#     green:   '0x9cf087'
#     yellow:  '0xffcc1b'
#     blue:    '0x7eb2dd'
#     magenta: '0xfb94ff'
#     cyan:    '0x00ffff'
#     white:   '0xb7cff9'

#   cursor:
#     text: "0x002635"
#     cursor: "0xffcc1b"
########  Dracula color theme ################
# Colors (Dracula)
# colors:
#   # Default colors
#   primary:
#     background: '0x282a36'
#     foreground: '0xf8f8f2'

#     # Bright and dim foreground colors
#     #
#     # The dimmed foreground color is calculated automatically if it is not present.
#     # If the bright foreground color is not set, or `draw_bold_text_with_bright_colors`
#     # is `false`, the normal foreground color will be used.
#     #dim_foreground: '0x9a9a9a'
#     #bright_foreground: '0xffffff'

#   # Cursor colors
#   #
#   # Colors which should be used to draw the terminal cursor. If these are unset,
#   # the cursor color will be the inverse of the cell color.
#   cursor:
#     text: '0x44475a'
#     cursor: '#F81CE5'

#   # Normal colors
#   normal:
#     black:   '0x000000'
#     red:     '0xff5555'
#     green:   '0x50fa7b'
#     yellow:  '0xf1fa8c'
#     blue:    '0xbd93f9'
#     magenta: '0xff79c6'
#     cyan:    '0x8be9fd'
#     white:   '0xbfbfbf'

#   # Bright colors
#   bright:
#     black:   '0x4d4d4d'
#     red:     '0xff6e67'
#     green:   '0x5af78e'
#     yellow:  '0xf4f99d'
#     blue:    '0xcaa9fa'
#     magenta: '0xff92d0'
#     cyan:    '0x9aedfe'
#     white:   '0xe6e6e6'

#   # Dim colors
#   #
#   # If the dim colors are not set, they will be calculated automatically based
#   # on the `normal` colors.
#   dim:
#     black:   '0x14151b'
#     red:     '0xff2222'
#     green:   '0x1ef956'
#     yellow:  '0xebf85b'
#     blue:    '0x4d5b86'
#     magenta: '0xff46b0'
#     cyan:    '0x59dffc'
#     white:   '0xe6e6d1'

#   # Indexed Colors
#   #
#   # The indexed colors include all colors from 16 to 256.
#   # When these are not set, they're filled with sensible defaults.
#   #
#   # Example:
#   #   `- { index: 16, color: '0xff00ff' }`
#   #
#   indexed_colors: []

######## Default color theme ###########################
# # Colors (Tomorrow Night Bright)
#colors:
  # Default colors
  #primary:
  #  background: '#000000'
  #  foreground: '#eaeaea'

    # Bright and dim foreground colors
    #
    # The dimmed foreground color is calculated automatically if it is not present.
    # If the bright foreground color is not set, or `draw_bold_text_with_bright_colors`
    # is `false`, the normal foreground color will be used.
    #dim_foreground: '#9a9a9a'
    #bright_foreground: '#ffffff'

  # Cursor colors
  #
  # Colors which should be used to draw the terminal cursor. If these are unset,
  # the cursor color will be the inverse of the cell color.
  #cursor:
  #  text: '#000000'
  #  cursor: '#ffffff'

  # Selection colors
  #
  # Colors which should be used to draw the selection area. If selection
  # background is unset, selection color will be the inverse of the cell colors.
  # If only text is unset the cell text color will remain the same.
  #selection:
  #  text: '#eaeaea'
  #  background: '#404040'

  # Normal colors
  #normal:
  #  black:   '#000000'
  #  red:     '#d54e53'
  #  green:   '#b9ca4a'
  #  yellow:  '#e6c547'
  #  blue:    '#7aa6da'
  #  magenta: '#c397d8'
  #  cyan:    '#70c0ba'
  #  white:   '#eaeaea'

  # Bright colors
  #bright:
  #  black:   '#666666'
  #  red:     '#ff3334'
  #  green:   '#9ec400'
  #  yellow:  '#e7c547'
  #  blue:    '#7aa6da'
  #  magenta: '#b77ee0'
  #  cyan:    '#54ced6'
  #  white:   '#ffffff'

  # Dim colors
  #
  # If the dim colors are not set, they will be calculated automatically based
  # on the `normal` colors.
  #dim:
  #  black:   '#000000'
  #  red:     '#8c3336'
  #  green:   '#7a8530'
  #  yellow:  '#97822e'
  #  blue:    '#506d8f'
  #  magenta: '#80638e'
  #  cyan:    '#497e7a'
  #  white:   '#9a9a9a'

  # Indexed Colors
  #
  # The indexed colors include all colors from 16 to 256.
  # When these are not set, they're filled with sensible defaults.
  #
  # Example:
  #   `- { index: 16, color: '#ff00ff' }`
  #
  #indexed_colors: []

# Visual Bell
#
# Any time the BEL code is received, Alacritty "rings" the visual bell. Once
# rung, the terminal background will be set to white and transition back to the
# default background color. You can control the rate of this transition by
# setting the `duration` property (represented in milliseconds). You can also
# configure the transition function by setting the `animation` property.
#
# Values for `animation`:
#   - Ease
#   - EaseOut
#   - EaseOutSine
#   - EaseOutQuad
#   - EaseOutCubic
#   - EaseOutQuart
#   - EaseOutQuint
#   - EaseOutExpo
#   - EaseOutCirc
#   - Linear
#
# Specifying a `duration` of `0` will disable the visual bell.
#visual_bell:
#  animation: EaseOutExpo
#  duration: 0
#  color: '#ffffff'

# Background opacity
#
# Window opacity as a floating point number from `0.0` to `1.0`.
# The value `0.0` is completely transparent and `1.0` is opaque.
#background_opacity: 1.0

#selection:
  #semantic_escape_chars: ",│`|:\"' ()[]{}<>\t"

  # When set to `true`, selected text will be copied to the primary clipboard.
  #save_to_clipboard: false

# Allow terminal applications to change Alacritty's window title.
#dynamic_title: true

#cursor:
  # Cursor style
  #
  # Values for `style`:
  #   - ▇ Block
  #   - _ Underline
  #   - | Beam
  #style: Block

  # If this is `true`, the cursor will be rendered as a hollow box when the
  # window is not focused.
  #unfocused_hollow: true

# Live config reload (changes require restart)
#live_config_reload: true

# Shell
#
# You can set `shell.program` to the path of your favorite shell, e.g. `/bin/fish`.
# Entries in `shell.args` are passed unmodified as arguments to the shell.
#
# Default:
#   - (macOS) /bin/bash --login
#   - (Linux/BSD) user login shell
#   - (Windows) powershell
#shell:
#  program: /bin/bash
#  args:
#    - --login

# Startup directory
#
# Directory the shell is started in. If this is unset, or `None`, the working
# directory of the parent process will be used.
#working_directory: None

# WinPTY backend (Windows only)
#
# Alacritty defaults to using the newer ConPTY backend if it is available,
# since it resolves a lot of bugs and is quite a bit faster. If it is not
# available, the the WinPTY backend will be used instead.
#
# Setting this option to `true` makes Alacritty use the legacy WinPTY backend,
# even if the ConPTY backend is available.
#winpty_backend: false

# Send ESC (\x1b) before characters when alt is pressed.
alt_send_esc: true

#mouse:
  # Click settings
  #
  # The `double_click` and `triple_click` settings control the time
  # alacritty should wait for accepting multiple clicks as one double
  # or triple click.
  #double_click: { threshold: 300 }
  #triple_click: { threshold: 300 }

  # If this is `true`, the cursor is temporarily hidden when typing.
  #hide_when_typing: false

  #url:
    # URL launcher
    #
    # This program is executed when clicking on a text which is recognized as a URL.
    # The URL is always added to the command as the last parameter.
    #
    # When set to `None`, URL launching will be disabled completely.
    #
    # Default:
    #   - (macOS) open
    #   - (Linux/BSD) xdg-open
    #   - (Windows) explorer
    #launcher:
    #  program: xdg-open
    #  args: []

    # URL modifiers
    #
    # These are the modifiers that need to be held down for opening URLs when clicking
    # on them. The available modifiers are documented in the key binding section.
    #modifiers: None

# Mouse bindings
#
# Mouse bindings are specified as a list of objects, much like the key
# bindings further below.
#
# To trigger mouse bindings when an application running within Alacritty captures the mouse, the
# `Shift` modifier is automatically added as a requirement.
#
# Each mouse binding will specify a:
#
# - `mouse`:
#
#   - Middle
#   - Left
#   - Right
#   - Numeric identifier such as `5`
#
# - `action` (see key bindings)
#
# And optionally:
#
# - `mods` (see key bindings)
#mouse_bindings:
#  - { mouse: Middle, action: PasteSelection }

# Key bindings
#
# Key bindings are specified as a list of objects. For example, this is the
# default paste binding:
#
# `- { key: V, mods: Control|Shift, action: Paste }`
#
# Each key binding will specify a:
#
# - `key`: Identifier of the key pressed
#
#    - A-Z
#    - F1-F24
#    - Key0-Key9
#
#    A full list with available key codes can be found here:
#    https://docs.rs/glutin/*/glutin/event/enum.VirtualKeyCode.html#variants
#
#    Instead of using the name of the keys, the `key` field also supports using
#    the scancode of the desired key. Scancodes have to be specified as a
#    decimal number. This command will allow you to display the hex scancodes
#    for certain keys:
#
#       `showkey --scancodes`.
#
# Then exactly one of:
#
# - `chars`: Send a byte sequence to the running application
#
#    The `chars` field writes the specified string to the terminal. This makes
#    it possible to pass escape sequences. To find escape codes for bindings
#    like `PageUp` (`"\x1b[5~"`), you can run the command `showkey -a` outside
#    of tmux. Note that applications use terminfo to map escape sequences back
#    to keys. It is therefore required to update the terminfo when changing an
#    escape sequence.
#
# - `action`: Execute a predefined action
#
#   - Copy
#   - Paste
#   - PasteSelection
#   - IncreaseFontSize
#   - DecreaseFontSize
#   - ResetFontSize
#   - ScrollPageUp
#   - ScrollPageDown
#   - ScrollLineUp
#   - ScrollLineDown
#   - ScrollToTop
#   - ScrollToBottom
#   - ClearHistory
#   - Hide
#   - Minimize
#   - Quit
#   - ToggleFullscreen
#   - SpawnNewInstance
#   - ClearLogNotice
#   - ReceiveChar
#   - None
#
#   (macOS only):
#   - ToggleSimpleFullscreen: Enters fullscreen without occupying another space
#
# - `command`: Fork and execute a specified command plus arguments
#
#    The `command` field must be a map containing a `program` string and an
#    `args` array of command line parameter strings. For example:
#       `{ program: "alacritty", args: ["-e", "vttest"] }`
#
# And optionally:
#
# - `mods`: Key modifiers to filter binding actions
#
#    - Command
#    - Control
#    - Option
#    - Super
#    - Shift
#    - Alt
#
#    Multiple `mods` can be combined using `|` like this:
#       `mods: Control|Shift`.
#    Whitespace and capitalization are relevant and must match the example.
#
# - `mode`: Indicate a binding for only specific terminal reported modes
#
#    This is mainly used to send applications the correct escape sequences
#    when in different modes.
#
#    - AppCursor
#    - AppKeypad
#    - Alt
#
#    A `~` operator can be used before a mode to apply the binding whenever
#    the mode is *not* active, e.g. `~Alt`.
#
# Bindings are always filled by default, but will be replaced when a new
# binding with the same triggers is defined. To unset a default binding, it can
# be mapped to the `ReceiveChar` action. Alternatively, you can use `None` for
# a no-op if you do not wish to receive input characters for that binding.
#
# If the same trigger is assigned to multiple actions, all of them are executed
# at once.
key_bindings:
  # (Windows, Linux, and BSD only)
  #- { key: V,        mods: Control|Shift, action: Paste            }
  #- { key: C,        mods: Control|Shift, action: Copy             }
  #- { key: Insert,   mods: Shift,         action: PasteSelection   }
  #- { key: Key0,     mods: Control,       action: ResetFontSize    }
  #- { key: Equals,   mods: Control,       action: IncreaseFontSize }
  #- { key: Add,      mods: Control,       action: IncreaseFontSize }
  #- { key: Subtract, mods: Control,       action: DecreaseFontSize }
  #- { key: Minus,    mods: Control,       action: DecreaseFontSize }

  # (Windows only)
  #- { key: Return,   mods: Alt,           action: ToggleFullscreen }

  - { key: B,         mods: Command,       chars: "\x1bb"                       }
  - { key: F,         mods: Command,       chars: "\x1bf"                       }
  # (macOS only)
  - { key: A,         mods: Command,       chars: "\x1ba"                       }
  #- { key: B,        mods: Command,       chars: "\x1bb"                       }
  #- { key: C,        mods: Command,       chars: "\x1bc"                       }
  - { key: D,         mods: Command,       chars: "\x1bd"                       }
  #- { key: E,        mods: Command,       chars: "\x1be"                       }
  #- { key: F,        mods: Command,       chars: "\x1bf"                       }
  - { key: G,         mods: Command,       chars: "\x1bg"                       }
  - { key: H,         mods: Command,       chars: "\x1bh"                       }
  - { key: I,         mods: Command,       chars: "\x1bi"                       }
  - { key: J,         mods: Command,       chars: "\x1bj"                       }
  - { key: K,         mods: Command,       chars: "\x1bk"                       }
  - { key: L,         mods: Command,       chars: "\x1bl"                       }
  - { key: M,         mods: Command,       chars: "\x1bm"                       }
  - { key: N,         mods: Command,       chars: "\x1bn"                       }
  - { key: O,         mods: Command,       chars: "\x1bo"                       }
  - { key: P,         mods: Command,       chars: "\x1bp"                       }
  - { key: Q,         mods: Command,       chars: "\x1bq"                       }
  - { key: R,         mods: Command,       chars: "\x1br"                       }
  - { key: S,         mods: Command,       chars: "\x1bs"                       }
  - { key: T,         mods: Command,       chars: "\x1bt"                       }
  - { key: U,         mods: Command,       chars: "\x1bu"                       }
  #- { key: V,        mods: Command,       chars: "\x1bv"                       }
  - { key: W,         mods: Command,       chars: "\x1bw"                       }
  - { key: X,         mods: Command,       chars: "\x1bx"                       }
  - { key: Y,         mods: Command,       chars: "\x1by"                       }
  - { key: Z,         mods: Command,       chars: "\x1bz"                       }
  - { key: A,         mods: Command|Shift, chars: "\x1bA"                       }
  - { key: B,         mods: Command|Shift, chars: "\x1bB"                       }
  - { key: C,         mods: Command|Shift, chars: "\x1bC"                       }
  - { key: D,         mods: Command|Shift, chars: "\x1bD"                       }
  - { key: E,         mods: Command|Shift, chars: "\x1bE"                       }
  - { key: F,         mods: Command|Shift, chars: "\x1bF"                       }
  - { key: G,         mods: Command|Shift, chars: "\x1bG"                       }
  - { key: H,         mods: Command|Shift, chars: "\x1bH"                       }
  - { key: I,         mods: Command|Shift, chars: "\x1bI"                       }
  - { key: J,         mods: Command|Shift, chars: "\x1bJ"                       }
  - { key: K,         mods: Command|Shift, chars: "\x1bK"                       }
  - { key: L,         mods: Command|Shift, chars: "\x1bL"                       }
  - { key: M,         mods: Command|Shift, chars: "\x1bM"                       }
  - { key: N,         mods: Command|Shift, chars: "\x1bN"                       }
  - { key: O,         mods: Command|Shift, chars: "\x1bO"                       }
  - { key: P,         mods: Command|Shift, chars: "\x1bP"                       }
  - { key: Q,         mods: Command|Shift, chars: "\x1bQ"                       }
  - { key: R,         mods: Command|Shift, chars: "\x1bR"                       }
  - { key: S,         mods: Command|Shift, chars: "\x1bS"                       }
  - { key: T,         mods: Command|Shift, chars: "\x1bT"                       }
  - { key: U,         mods: Command|Shift, chars: "\x1bU"                       }
  - { key: V,         mods: Command|Shift, chars: "\x1bV"                       }
  - { key: W,         mods: Command|Shift, chars: "\x1bW"                       }
  - { key: X,         mods: Command|Shift, chars: "\x1bX"                       }
  - { key: Y,         mods: Command|Shift, chars: "\x1bY"                       }
  - { key: Z,         mods: Command|Shift, chars: "\x1bZ"                       }
  - { key: Key1,      mods: Command,       chars: "\x1b1"                       }
  - { key: Key2,      mods: Command,       chars: "\x1b2"                       }
  - { key: Key3,      mods: Command,       chars: "\x1b3"                       }
  - { key: Key4,      mods: Command,       chars: "\x1b4"                       }
  - { key: Key5,      mods: Command,       chars: "\x1b5"                       }
  - { key: Key6,      mods: Command,       chars: "\x1b6"                       }
  - { key: Key7,      mods: Command,       chars: "\x1b7"                       }
  - { key: Key8,      mods: Command,       chars: "\x1b8"                       }
  - { key: Key9,      mods: Command,       chars: "\x1b9"                       }
  - { key: Key0,      mods: Command,       chars: "\x1b0"                       }
  - { key: Space,     mods: Control,       chars: "\x00"                        } # Ctrl + Space
  - { key: Grave,     mods: Command,       chars: "\x1b`"                       } # Command + `
  - { key: Grave,     mods: Command|Shift, chars: "\x1b~"                       } # Command + ~
  - { key: Period,    mods: Command,       chars: "\x1b."                       } # Command + .
  - { key: Key8,      mods: Command|Shift, chars: "\x1b*"                       } # Command + *
  - { key: Key3,      mods: Command|Shift, chars: "\x1b#"                       } # Command + #
  - { key: Period,    mods: Command|Shift, chars: "\x1b>"                       } # Command + >
  - { key: Comma,     mods: Command|Shift, chars: "\x1b<"                       } # Command + <
  - { key: Minus,     mods: Command|Shift, chars: "\x1b_"                       } # Command + _
  - { key: Key5,      mods: Command|Shift, chars: "\x1b%"                       } # Command + %
  - { key: Key6,      mods: Command|Shift, chars: "\x1b^"                       } # Command + ^
  - { key: Backslash, mods: Command,       chars: "\x1b\\"                      } # Command + \
  - { key: Backslash, mods: Command|Shift, chars: "\x1b|"                       } # Command + |
  - { key: K,         mods: Command,       chars: "\x0c"                        }

  - { key: Key0,      mods: Command,         action: ResetFontSize              }
  - { key: Equals,    mods: Command,         action: IncreaseFontSize           }
  #- { key: Add,      mods: Command,         action: IncreaseFontSize           }
  - { key: Minus,     mods: Command,         action: DecreaseFontSize           }
  - { key: K,         mods: Command,         action: ClearHistory               }
  - { key: V,         mods: Command,         action: Paste                      }
  - { key: C,         mods: Command,         action: Copy                       }
  - { key: H,         mods: Command,         action: Hide                       }
  - { key: M,         mods: Command,         action: Minimize                   }
  - { key: Q,         mods: Command,         action: Quit                       }
  - { key: W,         mods: Command,         action: Quit                       }
  - { key: F,         mods: Command|Control, action: ToggleFullscreen           }

  - { key: Paste,                            action: Paste                      }
  - { key: Copy,                             action: Copy                       }
  #- { key: L,        mods: Control,         action: ClearLogNotice             }
  #- { key: L,        mods: Control,       chars: "\x0c"                        }
  #- { key: PageUp,   mods: Shift,           action: ScrollPageUp,   mode: ~Alt }
  #- { key: PageDown, mods: Shift,           action: ScrollPageDown, mode: ~Alt }
  #- { key: Home,     mods: Shift,           action: ScrollToTop,    mode: ~Alt }
  #- { key: End,      mods: Shift,           action: ScrollToBottom, mode: ~Alt }

#debug:
  # Display the time it takes to redraw each frame.
  #render_timer: false

  # Keep the log file after quitting Alacritty.
  #persistent_logging: false

  # Log level
  #
  # Values for `log_level`:
  #   - None
  #   - Error
  #   - Warn
  #   - Info
  #   - Debug
  #   - Trace
  #log_level: Warn

  # Print all received window events.
  #print_events: false
