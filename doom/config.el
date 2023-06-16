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
     doom-theme 'modus-operandi-tinted)
  (setq
   ;; doom-font (font-spec :family "monaco" :size 15 :weight 'normal)
   ;; doom-font (font-spec :family "JetBrains Mono" :size 19 :weight 'normal :width 'normal)
   ;; doom-variable-pitch-font (font-spec :family "Avenir Next" :size 21)
   doom-font (font-spec :family "Iosevka" :size 19 :weight 'normal)
   doom-big-font (font-spec :family "Iosevka" :size 25)
   doom-theme 'modus-vivendi-tinted))

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
 ;; org-startup-folded 'content
 org-startup-folded 'show2levels
 org-auto-align-tags nil
 org-roam-v2-ack t
 +org-roam-open-buffer-on-find-file nil
 sync-dir "~/Sync/"
 org-directory (concat sync-dir "org")
 org-spotify-directory (concat org-directory "/spotify")
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
 +org-capture-todo-file "tasks.org"
 org-exploration-file (concat org-directory
                              "/"
                              "exploration.org")
 org-design-file (concat org-directory
                         "/"
                         "design.org"))

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

(after! org-capture
  (pushnew! org-capture-templates
            '("e" "Explore domain" entry
              (file+headline org-exploration-file "Inbox")
              "* domain: %? \n** concepts\n** concepts relations\n** implications\n** problem statement\n" :prepend t))
  (pushnew! org-capture-templates
            '("d" "Design problem space" entry
              (file+headline org-design-file "Inbox")
              "* domain: %? \n** observe situation\n** diagnose possible problems\n** delimit the problem you are going to solve\n** approaches to the problem\n** implementation tactics\n** develop\n" :prepend t)))

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
       :desc "Integration test mvn project"  "i" (cmd! (beno--run-mvn-command "clean integration-test"))
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

(after! dap-java
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))

  (dap-register-debug-template "Custom Runner"
                               (list :type "java"
                                     :request "launch"
                                     :args ""
                                     :vmArgs "-ea -Dmyapp.instance.name=myapp_1"
                                     :projectName "sp"
                                     :classPaths nil
                                     :mainClass ""
                                     :env '(("DEV" . "1")))))

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
            mu4e-path (concat mu-root "/share/emacs/site-lisp/mu4e"))
           (add-to-list 'load-path mu4e-path)))

(after! mu4e
  (setq mu4e-update-interval 180))
(setq +mu4e-workspace-name "*mail*")

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

;; this won't work temporarily for protonmail as certificates are being moved to /etc/ssl/certs
(after! gnutls
  (add-to-list 'gnutls-trustfiles "~/.config/certificates/protonmail.crt"))

;; (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart)

(setq mu4e-bookmarks
      '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
        (:name "Skip messages" :query "(flag:unread AND NOT flag:trashed) AND (subject:\"Use Spotify BOM\" OR subject:\"Use Spotify root\" OR subject:\"no review needed\" OR from:\"fleet-manager-bot\" OR from:\"fleetshift-bot\" OR from:\"setadel-manager-bot\" OR from:\"setadel-bot\" OR subject:\"Update dependent library\")" :key 115)
        (:name "Today's messages" :query "date:today..now" :key 116)
        (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
        (:name "Messages with images" :query "mime:image/*" :key 112)
        (:name "Fragomen" :query "fragomen" :hide-unread t :key 102)))

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
  ("benouattara" (pushnew! auth-sources benny-secrets)))

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

(after! compile
  (compilation-set-skip-threshold 2)) ;; skip warning an info

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
  (if (s-contains? "Test.java" test-name) ;; surefire unit test
      (format "clean -DfailIfNoTests=false -Dtest=%s test" test-name)
    ;; failsafe integration test
    (format "clean -DfailIfNoTests=false -Dit.test=%s verify" test-name)))

(setq
 projectile-project-search-path '(("~/Code/" . 1) ("~/common-lisp" . 1) ("~/Code/archives/Code" . 1)))

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

(if (> (display-pixel-width) 1600)
    ;; large display
    (progn
      (set-popup-rule! +main-eshell-popup+ :size 0.40 :vslot -4 :select t :quit nil :ttl t :side 'right)
      (set-popup-rule! "*SQL:" :size 0.40 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
      (set-popup-rule! "^\\*Soccer.*" :size 0.33 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
      (set-popup-rule! "^\\*com.spotify.*" :size 0.40 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
      (set-popup-rule! "^\\*compilation.*" :size 0.40 :vslot -4 :select t :quit nil :ttl t :side 'right)
      (set-popup-rule! "^\\*Shell Command.*" :size 0.40 :vslot -4 :select t :quit t :ttl t :side 'right)
      (set-popup-rule! "^\\*helpful.*" :size 0.40 :vslot -4 :select t :quit nil :ttl t :side 'right)
      (set-popup-rule! "^\\*eww*" :size 0.40 :vslot -4 :select t :quit nil :ttl t :side 'right)
      (set-popup-rule! "^\\*Org Agenda\\*" :size 0.40 :vslot -4 :select t :quit nil :ttl t :side 'right)
      (set-popup-rule! "[0-9]+-[0-9]+-[0-9]+.org" :size 0.40 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
      (set-popup-rule! "journal.org" :size 0.40 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
      (set-popup-rule! "^[0-9]\\{8\\}$" :size 0.40 :vslot -4 :select t :quit 'other :ttl nil :side 'right :autosave t)
      (set-popup-rule! "*kubel" :size 0.50 :vslot -4 :select t :quit nil :ttl t :side 'right)
      (set-popup-rule! "^\\*sbt*" :size 0.40 :vslot -4 :select t :quit nil :ttl nil :side 'right)
      (set-popup-rule! "^\\*cider-repl" :size 0.40 :vslot -4 :select t :quit nil :ttl nil :side 'right)
      (set-popup-rule! "^\\*ChatGPT*" :size 0.40 :vslot -4 :select t :quit nil :ttl nil :side 'right)
      (set-popup-rule! "^\\*cider-inspect*" :size 0.40 :vslot -4 :select t :quit nil :ttl nil :side 'right))
  ;; small display
  (progn
    (set-popup-rule! +main-eshell-popup+ :size 0.35 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
    (set-popup-rule! "*SQL:" :size 0.35 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
    (set-popup-rule! "^\\*Soccer.*" :size 0.35 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
    (set-popup-rule! "^\\*compilation.*" :size 0.35 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
    (set-popup-rule! "^\\*Shell Command.*" :size 0.35 :vslot -4 :select t :quit t :ttl t :side 'bottom)
    (set-popup-rule! "^\\*helpful.*" :size 0.35 :vslot -4 :select nil :quit nil :ttl t :side 'bottom)
    (set-popup-rule! "^\\*eww*" :size 0.35 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
    ;; (set-popup-rule! "^\\*Org Agenda\\*" :size 0.25 :vslot -4 :select t :quit nil :ttl t :side 'right)
    ;; (set-popup-rule! "[0-9]+-[0-9]+-[0-9]+.org" :size 0.25 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
    ;; (set-popup-rule! "journal.org" :size 0.25 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
    ;; (set-popup-rule! "^[0-9]\\{8\\}$" :size 0.25 :vslot -4 :select t :quit 'other :ttl 5 :side 'right :autosave t)
    (set-popup-rule! "*kubel" :size 0.35 :vslot -4 :select t :quit nil :ttl t :side 'bottom)
    (set-popup-rule! "^\\*sbt*" :size 0.35 :vslot -4 :select t :quit nil :ttl nil :side 'bottom)
    (set-popup-rule! "^\\*cider-repl" :size 0.35 :vslot -4 :select t :quit nil :ttl nil :side 'bottom)
    (set-popup-rule! "^\\*ChatGPT*" :size 0.35 :vslot -4 :select t :quit nil :ttl nil :side 'bottom)
    (set-popup-rule! "^\\*cider-inspect*" :size 0.35 :vslot -4 :select t :quit nil :ttl nil :side 'bottom)))

;; (vertico-posframe-mode 1)
;; (setq vertico-posframe-parameters
;;       '((left-fringe . 8)
;;         (right-fringe . 8)))

(add-to-list 'default-frame-alist '(undecorated . t))

(after! elfeed
  (setq elfeed-search-filter "@2-weeks-ago +unread"))
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

(setq beno-custom-lib "~/Code/dotfiles/lib/")
(add-to-list 'load-path beno-custom-lib)
(require 'soccer)
(map! :leader
          (:prefix-map ("o" . "open")
           (:prefix ("S" . "soccer")
            :desc "Favorite fixtures" "S" #'list-soccer-fixtures
            :desc "Followed leagues" "l" #'list-soccer-leagues
            :desc "Followed teams" "t" #'list-soccer-teams
            :desc "Teams fixtures" "T" #'list-soccer-team-fixtures
            :desc "Follow league" "f" #'soccer-follow-league
            :desc "Unfollow league" "U" #'soccer-unfollow-league
            :desc "Unfollow team" "u" #'soccer-unfollow-team
            :desc "Follow team" "F" #'soccer-follow-team)))

(after! eww
  (eww-toggle-fonts))

(defun beno-gpt-key ()
  "Read gpt secret from authsource."
  (funcall (plist-get (car (auth-source-search :host gpt-api-key))
                      :secret)))
(map! :leader
      :desc "gpt"
      :n "o g"
      'gptel)

(after! gptel
  (setq gpt-api-key "api.openai.com"
        gptel-default-mode 'org-mode
        gptel-api-key #'beno-gpt-key))

(after! cider
  (add-hook 'before-save-hook 'cider-format-buffer t t))
