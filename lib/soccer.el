;;; soccer.el -- Manage upcoming soccer Fixtures -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ben O
;;
;; Author: Ben O <benny.ouattara@gmail.com>
;; Maintainer: Ben O <benny.ouattara@gmail.com>
;; Created: February 18, 2023
;; Modified: February 18, 2023
;; Version: 0.0.1
;; Keywords: soccer, football, fixtures
;; Homepage: https://github.com/benny-ouattara/soccer.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'cl-lib)

(defvar soccer-base-url "https://api-football-v1.p.rapidapi.com"
  "Base football API that provides past and realtime soccer data.")

(defvar soccer-season 2022
  "Soccer season to explore.")

(defvar soccer-tz "America/New_York"
  "Date and time timezone.")

(defvar soccer-debug t
  "Turn debugging on and off.")

(defvar soccer-api-log-template (concat (make-string 20 ?=)
                                        "API CALL: %s"
                                        (make-string 20 ?=))
  "API calls header template.")

(defvar soccer-fixtures-limit 25
  "Max number of upcoming fixtures to fetch.")

(defvar soccer-team-fixtures-url-template
  "/v3/fixtures?season=%s&team=%d&timezone=%s&next=%d"
  "Fixtures URL path template")

(defvar soccer-log-buffer "*soccer-log-buffer*"
  "Log buffer name.")

(defvar soccer-api-call-buffer  "*api-call*"
  "API call log buffer name.")

(defvar soccer-api-key  "api.football.com"
  "Authsource backend entry to retrieve API secret key.")

(defvar soccer-leagues-buffer "*Soccer Leagues*"
  "Leagues buffer name.")

(defvar soccer-teams-buffer "*Soccer Teams*"
  "Teams buffer name.")

(defvar soccer-fixtures-buffer "*Soccer Fixtures*"
  "Fixtures buffer name.")

(defvar soccer-favorite-fixtures-buffer "*Soccer Favorites*"
  "Favorite fixtures buffer name.")

(defvar soccer-local-store-path (concat doom-cache-dir
                                        (concat "soccer"
                                                "/"
                                                (int-to-string soccer-season)))
  "Soccer db path prefixed by season.")

(defconst soccer-local-store-leagues-path (concat soccer-local-store-path
                                                  "/leagues")
  "Leagues db path.")

(defconst soccer-local-store-teams-path (concat soccer-local-store-path
                                                "/teams")
  "Teams db path.")

(defvar soccer-leagues-list-url (concat soccer-base-url
                                        "/v3/leagues")
  "Endpoint for all leagues worldwide.")

(defvar soccer-local-store-leagues-all-path (concat soccer-local-store-leagues-path
                                                    "/all.data")
  "Absolute path where all leagues are stored.")

(defun soccer-local-store-teams-all-path (league-id)
  "Determines absolute path where all teams belonging to LEAGUE-ID are stored."
  (concat soccer-local-store-teams-path
          "/"
          (int-to-string league-id)
          "-all.data"))

(defvar soccer-local-store-leagues-followed-path (concat soccer-local-store-leagues-path
                                                         "/followed.data")
  "Absolute path where followed leagues are stored.")

(defvar soccer-local-store-teams-followed-path (concat soccer-local-store-teams-path
                                                         "/followed.data")
  "Absolute path where followed teams are stored.")

(defvar soccer-league-name-delimiter " - "
  "Delimiter between league name and league country.")

(defvar soccer-league-download-message
  "No leagues found locally, downloading will take a while. Would you like to proceed?"
  "Prompt to get user consent for download.")

(defvar soccer-downloaded-leagues-path (concat soccer-local-store-teams-path
                                               "/downloaded-leagues.data")
  "Absolute path where already downloaded leagues's teams are stored.")

;; models
(cl-defstruct (soccer-team (:constructor soccer-team--create))
  "Represent a soccer team."
  id name code country founded logo)

(cl-defstruct (soccer-fixture (:constructor soccer-fixture--create))
  "Represent a soccer fixture."
  timestamp venue status league round home away)

(cl-defstruct (soccer-league (:constructor soccer-league--create))
  "Represent a soccer league."
  id name type logo country)

(defun soccer-leagues-url (league-id)
  "Return the URL where to fetch the LEAGUE corresponding to LEAGUE-ID."
  (concat soccer-base-url (format "/v3/leagues?id=%d&season=%s"
                                  league-id
                                  soccer-season)))

(defun soccer-teams-url (league-id)
  "Return the URL where to fetch the TEAMS of the LEAGUE-ID for the SEASON."
  (concat soccer-base-url (format "/v3/teams?league=%s&season=%d"
                                  league-id
                                  soccer-season)))

(defun soccer-team-fixtures-url (team-id &optional limit)
  "Return the URL where to fetch the FIXTURES ot the TEAM-ID."
  (concat soccer-base-url (format soccer-team-fixtures-url-template
                                  soccer-season
                                  team-id
                                  soccer-tz
                                  (or limit soccer-fixtures-limit))))

(defun soccer-league-filename (league-id)
  "Determine specific league absolute path."
  (concat soccer-local-store-leagues-path
          "/"
          (int-to-string league-id)
          ".data"))

(defun soccer-team-filename (team-id)
  "Determine specific team absolute path."
  (concat soccer-local-store-teams-path
          "/"
          (int-to-string team-id)
          ".data"))

(defun soccer-parse-teams (payload)
  "Parse soccer team from the PAYLOAD that is an alist of the teams API response."
  (let ((raw-teams (append (alist-get 'response payload) nil)))
    (cl-loop for raw-team in raw-teams
             collect (let* ((team (alist-get 'team raw-team)))
                       (soccer-team--create :id (alist-get 'id team)
                                            :name (alist-get 'name team)
                                            :code (alist-get 'code team)
                                            :country (alist-get 'country team)
                                            :founded (alist-get 'founded team)
                                            :logo (alist-get 'logo team))))))

(defun soccer-fetch-teams (league-id)
  "Fetch all the teams in a LEAGUE-ID.
Return an alist of (TEAM-NAME . TEAM)."
  (if-let* ((url (soccer-teams-url league-id))
            (path (soccer-local-store-teams-all-path league-id))
            (cached (file-exists-p path)))
      (soccer-load-file path)
    (soccer--log (format "Team cache miss for league %d, remote call made" league-id))
    (let* ((teams  (soccer-parse-teams (soccer-fetch-url url)))
           (pairs (-map (lambda (team)
                                (cons (soccer-team-name team)
                                      team))
                              teams)))
      (soccer--write-to (soccer-local-store-teams-all-path league-id)
                        pairs)
      (dolist (team teams)
        (soccer--write-to (soccer-team-filename (soccer-team-id team))
                          (cons (soccer-team-name team)
                                team)))
      pairs)))

(defun soccer-fetch-fixtures (team-id &optional limit)
  "Fetch upcoming fixtures of TEAM-ID.
No caching is applied on fixtures since they are dynamic and change on a daily or even hourly basis.
We want to always see the latest and greatest in fixture details."
  (let* ((url (soccer-team-fixtures-url team-id limit))
         (json (soccer-fetch-url url))
         (response (append (alist-get 'response json) nil)))
    (cl-loop for r in response
             collect (let* ((fixture (alist-get 'fixture r))
                            (teams (alist-get 'teams r))
                            (home (alist-get 'name (alist-get 'home teams)))
                            (away (alist-get 'name (alist-get 'away teams)))
                            (league (alist-get 'league r))
                            (league-name (alist-get 'name league))
                            (league-round (alist-get 'round league))
                            (venue (alist-get 'name (alist-get 'venue fixture)))
                            (timestamp (alist-get 'timestamp fixture))
                            (status (alist-get 'long (alist-get 'status fixture))))
                       (soccer-fixture--create :timestamp timestamp
                                               :venue venue
                                               :status status
                                               :league league-name
                                               :round league-round
                                               :home home
                                               :away away)))))

(defun soccer--write-to (filepath content)
  "Write CONTENT to FILEPATH.

FILEPATH is the absolute path of the destination file.
CONTENT can be any elisp object."
  (with-temp-buffer
    (insert (format "%S" content))
    (write-file filepath)))

(defun soccer-data-path (dirpath)
  "Return cache payload at DIRPATH.

DIRPATH is a directory that stores `.data' extended files which contain alist of soccer API responses.
It returns a list of (FILENAME<.data> . ABSOLUTE-FILEPATH) found at DIRPATH."
  (unless (file-directory-p dirpath)
    (mkdir dirpath t))
  (mapcar (lambda (filepath)
            (cons (string-to-number (downcase (file-name-base filepath)))
                  filepath))
          (f-files dirpath (lambda (filename)
                             (s-ends-with? ".data" filename)))))

(defun soccer-data-paths ()
  "Return list of all (FILENAME . FILEPATH) in database.

fILEPATH contains a soccer endpoint API response.
FILENAME is a symbol representing the file.
It creates the base databse directory if it doesn't exist alongside its subdirectories."
  (let* ((leagues-raw (soccer-data-path soccer-local-store-leagues-path))
         (teams-raw (soccer-data-path soccer-local-store-teams-path)))
    `((leagues . ,leagues-raw) (teams . ,teams-raw))))

(defun soccer-load-file (filename)
  "Return content stored in FILENAME.
The content of FILENAME is expected to be an alist of soccer API response.
It makes sure to return a list and not a vector."
  (when (file-exists-p filename)
    (let ((raw (with-current-buffer (find-file-noselect filename)
                 (goto-char (point-min))
                 (read (buffer-string)))))
      raw)))

(defun soccer-fetch-url (url)
  "Fetch content at URL.
Assumes that response is json formatted and tries to read body into an alist with (json-read).
The API secret is retrieved from `.authinfo.gpg' or whatever backend that is preferred by `authsource'.
Logs the full API response if `soccer-debug' is non nil."
  (let* ((url-request-method "GET")
         (url-user-agent (format "%s Agent" user-full-name))
         (url-request-extra-headers `(("X-Rapidapi-Key" .
                                       ,(funcall
                                         (plist-get (car
                                                     (auth-source-search :host soccer-api-key))
                                                    :secret)))
                                      ("X-Rapidapi-Host" . "api-football-v1.p.rapidapi.com")))
         (response-buffer (url-retrieve-synchronously url 'silent 'inhibit-cookies)))
    (when soccer-debug
      (with-current-buffer (get-buffer-create soccer-api-call-buffer)
        (insert (format soccer-api-log-template url))
        (insert-buffer response-buffer)
        (newline)))
    (with-current-buffer response-buffer
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun soccer--log (content)
  "Log string content to buffer specified by `soccer-log-buffer' when `soccer-debug' is non nil."
  (when soccer-debug
    (with-current-buffer (get-buffer-create soccer-log-buffer nil)
      (insert content)
      (newline))))

(defun soccer-downloaded-leagues-p ()
  "Determines whether leagues have been downloaded locally."
  (soccer-data-path soccer-local-store-leagues-path))

(defun soccer--fetch-all-leagues ()
  "Fetch and store all leagues worldwide.
If leagues are present in the local database, return it.
Otherwise retrieve leagues from remote, store them locally and return it.
Leagues are stored as an alist of (LEAGUE-NAME . SOCCER-LEAGUE) in
individual files in `soccer-local-store-leagues-path'."
  (unless (soccer-downloaded-leagues-p)
    (let* ((payload (soccer-fetch-url soccer-leagues-list-url))
           (raw-response (append (alist-get 'response payload) nil))
           (parsed-leagues (cl-loop for response in raw-response
                                    collect (let* ((league (alist-get 'league response))
                                                   (country (alist-get 'country response))
                                                   (country-name (alist-get 'name country))
                                                   (id (alist-get 'id league))
                                                   (name (alist-get 'name league))
                                                   (type (alist-get 'type league))
                                                   (logo (alist-get 'logo league)))
                                              (soccer-league--create :id id
                                                                     :name name
                                                                     :type type
                                                                     :country country-name
                                                                     :logo logo))))
           (pairs  (cl-loop for league in parsed-leagues
                            collect (cons (concat  (soccer-league-name league)
                                                   soccer-league-name-delimiter
                                                   (soccer-league-country league))
                                          league))))
      (soccer--log "No leagues in datastore, fetch from remote and store.")
      (soccer--write-to soccer-local-store-leagues-all-path
                        pairs)
      (dolist (pair pairs)
        (let* ((league (cdr pair))
               (league-id (soccer-league-id league)))
          (soccer--write-to (soccer-league-filename league-id)
                            pair)))))
  (let* ((raw-leagues (soccer-load-file soccer-local-store-leagues-all-path)))
    raw-leagues))

(defun soccer--fetch-all-teams ()
  "Fetch and store all teams of `followed-leagues' for the given season.
If league's teams is already present, return them.
Otherwise retrieve teams from remote, store them and return it.
Team are stored as an alist of (TEAM-NAME . SOCCER-TEAM) in
individual files in `soccer-local-store-teams-path'."
  (let* ((followed-leagues (soccer--followed-leagues))
         (downloaded-leagues (soccer-load-file soccer-downloaded-leagues-path))
         (result))
    (dolist (followed followed-leagues)
      (unless (assoc (car followed) downloaded-leagues)
        (soccer-fetch-teams (soccer-league-id (cdr followed)))
        (push followed downloaded-leagues)
        (soccer--write-to soccer-downloaded-leagues-path downloaded-leagues)))
    (dolist (followed followed-leagues result)
      (push (soccer-fetch-teams (soccer-league-id (cdr followed)))
              result))
    (-flatten result)))

(defun soccer--followed-leagues ()
  "If there are followed leagues, return alist of (LEAGUE-NAME . SOCCER-LEAGUE).
Load alist from local database at `soccer-local-store-leagues-path/followed.data'."
  (let* ((followed (soccer-load-file soccer-local-store-leagues-followed-path)))
    followed))

(defun soccer--followed-teams ()
  "If there are followed teams, return alist of (TEAM-NAME . SOCCER-TEAM).
Load alist from local database at `soccer-local-store-leagues-path/followed.data'."
  (let* ((followed (soccer-load-file soccer-local-store-teams-followed-path)))
    followed))

(defun soccer--fetch-favorite-fixtures (limit)
  "Fetch upcoming fixtures for teams defined in `soccer-followed-teams'.
If non nil, gets up to LIMIT fixtures per team, otherwise gets only one upcoming fixture."
  (let* ((result)
         (teams (-map #'cdr (soccer--followed-teams))))
    (dolist (team teams result)
      (let* ((team-fixtures (soccer-fetch-fixtures (soccer-team-id team)
                                                   limit))
             (ordered-fixtures (-sort (lambda (x y)
                                        (> (soccer-fixture-timestamp x)
                                           (soccer-fixture-timestamp y)))
                                      team-fixtures))
             (chosen-fixtures (-take limit ordered-fixtures)))
        (push chosen-fixtures result)))
    (-sort (lambda (x y)
             (> (soccer-fixture-timestamp x)
                (soccer-fixture-timestamp y)))
           (-flatten result))))

(defun soccer--download-and-follow-league (league-name)
  "Interactively select LEAGUE-NAME and tracks it as a `soccer-local-store-leagues-path/followed.data'.
Teams and upcoming fixtures are derived from `soccer-followed-leagues'."
  (interactive (list (completing-read "Follow league: "
                                      (mapcar #'car (soccer--fetch-all-leagues)))))
  (let* ((all-leagues (soccer--fetch-all-leagues))
         (current-leagues (soccer-load-file soccer-local-store-leagues-followed-path))
         (indexed-league (assoc league-name all-leagues)))
    (unless (assoc league-name current-leagues)
      (push indexed-league current-leagues)
      (soccer--write-to soccer-local-store-leagues-followed-path
                        current-leagues))))

(defun soccer-follow-league ()
  "Presents a prompt before proceeding with downloading missing leagues."
  (interactive)
  (if (soccer-downloaded-leagues-p)
      (call-interactively #'soccer--download-and-follow-league)
    (and (yes-or-no-p soccer-league-download-message)
         (call-interactively #'soccer--download-and-follow-league))))

(defun soccer-follow-team (team-name)
  "Interactively select TEAM-NAME to follow and tracks it as `soccer-local-store-teams-path/followed.data'.
These are considered as favorite teams and their next fixtures can be queried."
  (interactive (list (completing-read "Follow team: "
                                      (mapcar #'car (soccer--fetch-all-teams)))))
  (let* ((all-teams (soccer--fetch-all-teams))
         (current-teams (soccer-load-file soccer-local-store-teams-followed-path))
         (indexed-team (assoc team-name all-teams)))
    (unless (assoc team-name current-teams)
      (push indexed-team current-teams)
      (soccer--write-to soccer-local-store-teams-followed-path
                        current-teams))))

(defun soccer-favorite-fixtures--entries (limit)
  "Populate table entries with upcoming fixtures for `followed' teams."
  (setq tabulated-list-entries nil)
  (let* ((fixtures (soccer--fetch-favorite-fixtures limit)))
    (dolist (fixture fixtures)
      (push (list fixture (vector (s-truncate 22 (soccer-fixture-league fixture))
                                  (format-time-string "%a, %b %d" (soccer-fixture-timestamp fixture))
                                  (format-time-string "%I:%M %p" (soccer-fixture-timestamp fixture))
                                  (s-truncate 22 (soccer-fixture-home fixture))
                                  (s-truncate 22 (soccer-fixture-away fixture))
                                  (s-truncate 17 (soccer-fixture-status fixture))
                                  (soccer-fixture-round fixture)))
            tabulated-list-entries))))

;;;###autoload
(define-derived-mode soccer-favorite-fixture-mode tabulated-list-mode "SoccerFavorite"
  "Mode to view upcoming fixtures of favorite teams."
  (setq tabulated-list-format (vector '("League" 22 t)
                                      '("Date" 12 t)
                                      '("Time" 10 t)
                                      '("Home" 22 t)
                                      '("Away" 22 t)
                                      '("Status" 15 t)
                                      '("Round" 0 t)))
  ;; (setq tabulated-list-sort-key (cons "Time" t))
  (tabulated-list-init-header))

;;;###autoload
(defun list-soccer-fixtures (limit &optional buff)
  "Entry point to list upcoming fixtures for `followed' teams."
  (interactive "p")
  (unless buff
    (setq buff (get-buffer-create soccer-favorite-fixtures-buffer)))
  (with-current-buffer soccer-favorite-fixtures-buffer
    (soccer-favorite-fixture-mode)
    (soccer-favorite-fixtures--entries limit) ;; default limit is 1 for (interactive "p")
    (tabulated-list-print))
  (display-buffer soccer-favorite-fixtures-buffer))

(defun soccer-leagues-visit-teams (button)
  "Action when a league is selected from the table.
Show all the teams that play in that league for that season."
  (let ((league (button-get button 'league)))
    (list-soccer-teams (soccer-league-id league))))

(defun soccer-leagues--entries ()
  "Populate league table entries with leagues info."
  (setq tabulated-list-entries nil)
  (let ((leagues (-map #'cdr (soccer--followed-leagues))))
    (dolist (league leagues)
      (push (list league (vector `(,(soccer-league-name league)
                                   face link
                                   action soccer-leagues-visit-teams
                                   help-echo ,(format-message "View `%s' teams"
                                                              (soccer-league-name league))
                                   follow-link t
                                   league ,league)
                                 (soccer-league-type league)
                                 (soccer-league-country league)))
            tabulated-list-entries))))

;;;###autoload
(define-derived-mode soccer-league-mode tabulated-list-mode "SoccerLeague"
  "Mode to view soccer leagues.
Right now, the leagues that are viewed are predefined selection that is hardcoded.
They represent the three most popular soccer leagues in the world.
Popularity is assessed based on reported viewership."
  (setq tabulated-list-format (vector '("Name" 45 t)
                                      '("Type" 35 t)
                                      '("Country" 0 t)))
  (add-hook 'tabulated-list-revert-hook 'soccer-leagues--entries nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun list-soccer-leagues (&optional buff)
  "Entry function to explore soccer teams and upcoming fixtures."
  (interactive)
  (unless buff
    (setq buff (get-buffer-create soccer-leagues-buffer)))
  (with-current-buffer soccer-leagues-buffer
    (soccer-league-mode)
    (soccer-leagues--entries)
    (tabulated-list-print))
  (display-buffer soccer-leagues-buffer))

(defun soccer-teams-visit-fixtures (button)
  "Action when a team is selected from the table.

Show all upcoming fixtures for that team.
The number of fixtures is capped by `soccer-fixtures-limit'."
  (let ((team (button-get button 'team)))
    (list-soccer-team-fixtures (soccer-team-id team))))

(defun soccer-teams--entries (league-id)
  "Populate table entries with the teams of the league identified by LEAGUE-ID."
  (setq tabulated-list-entries nil)
  (let ((teams (-map #'cdr (soccer-fetch-teams league-id))))
    (dolist (team teams)
      (push (list team (vector `(,(soccer-team-name team)
                                 face link
                                 action soccer-teams-visit-fixtures
                                 help-echo ,(format-message "View `%s' team fixtures"
                                                            (soccer-team-name team))
                                 follow-link t
                                 team ,team)
                               (or (soccer-team-code team)
                                   "N/A")
                               (or (soccer-team-country team)
                                   "N/A")
                               (if (soccer-team-founded team)
                                   (int-to-string (soccer-team-founded team))
                                 "N/A")))
            tabulated-list-entries))))

;;;###autoload
(define-derived-mode soccer-team-mode tabulated-list-mode "SoccerTeam"
  "Mode to view teams within a league."
  (setq tabulated-list-format (vector '("Name" 25 t)
                                      '("Code" 15 t)
                                      '("Country" 15 t)
                                      '("Founded" 0 t)))
  (tabulated-list-init-header))

;;;autoload
(defun list-soccer-teams (league-id &optional buff)
  "Entry point to list all soccer teams playing in LEAGUE-ID.
It reads the desired league from the minibuffer if none is provided."
  (interactive (list (let ((league-name (completing-read "Choose league: "
                                                         (mapcar #'car
                                                                 (soccer--fetch-all-leagues)))))
                       (soccer-league-id (cdr (assoc league-name (soccer--fetch-all-leagues)))))))
  (unless buff
    (setq buff (get-buffer-create soccer-teams-buffer)))
  (with-current-buffer soccer-teams-buffer
    (soccer-team-mode)
    (soccer-teams--entries league-id)
    (tabulated-list-print))
  (display-buffer soccer-teams-buffer))

(defun soccer-fixtures--entries (team-id)
  "Populate table entries with upcoming fixtures for team identified by TEAM-ID.
TEAM-ID are not known in advance, they are retrieved from the leagues.
The fixtures are sorted in ascending order of schedule."
  (setq tabulated-list-entries nil)
  (let ((fixtures (-sort (lambda (x y)
                           (>  (soccer-fixture-timestamp x)
                               (soccer-fixture-timestamp y)))
                         (soccer-fetch-fixtures team-id))))
    (dolist (fixture fixtures)
      (push (list fixture (vector (s-truncate 22 (soccer-fixture-league fixture))
                                  (format-time-string "%a, %b %d" (soccer-fixture-timestamp fixture))
                                  (format-time-string "%I:%M %p" (soccer-fixture-timestamp fixture))
                                  ;; (s-truncate 12 (soccer-fixture-venue fixture))
                                  (s-truncate 22 (soccer-fixture-home fixture))
                                  (s-truncate 22 (soccer-fixture-away fixture))
                                  (s-truncate 17 (soccer-fixture-status fixture))
                                  (soccer-fixture-round fixture)))
            tabulated-list-entries))))

;;;###autoload
(define-derived-mode soccer-fixture-mode tabulated-list-mode "SoccerFixture"
  "Mode to view upcoming fixtures of a team."
  (setq tabulated-list-format (vector '("League" 22 t)
                                      '("Date" 12 t)
                                      '("Time" 10 t)
                                      '("Home" 22 t)
                                      '("Away" 22 t)
                                      '("Status" 17 t)
                                      '("Round" 0 t)))
  ;; (setq tabulated-list-sort-key (cons "Time" t))
  (tabulated-list-init-header))

;;;###autoload
(defun list-soccer-team-fixtures (team-id &optional buff)
  "Entry point to list upcoming fixtures of a team identified by TEAM-ID."
  (interactive (list (let* ((teams (soccer--fetch-all-teams))
                            (team-name  (completing-read "Team: "
                                                         (-map #'car teams))))
                       (soccer-team-id (cdr (assoc team-name teams))))))
  (unless buff
    (setq buff (get-buffer-create soccer-fixtures-buffer)))
  (with-current-buffer soccer-fixtures-buffer
    (soccer-fixture-mode)
    (soccer-fixtures--entries team-id)
    (tabulated-list-print))
  (display-buffer soccer-fixtures-buffer))

(provide 'soccer)
;;; soccer.el ends here
