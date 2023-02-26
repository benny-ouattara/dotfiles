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

;; NOTE: to get the league id and name, checkout out this endpoint GET /v3/leagues
(defvar leagues '((:name "Premier League" :id 39)
                  (:name "Primera Division" :id 140)
                  (:name "Ligue 1" :id 61))
  "The leagues that can be explored.")

(defvar soccer-fixtures-limit 25
  "Max number of upcoming fixtures to fetch.")

(defvar soccer-team-fixtures-url-template "/v3/fixtures?season=%s&team=%d&timezone=%s&next=%d"
  "Fixtures URL path template")

(defvar soccer-log "*soccer-log*"
  "Log buffer name.")

(defvar soccer-api-key  "api.football.com"
  "authsource backend entry to retrieve API secret key.")

(defvar soccer-leagues-buffer "*Soccer Leagues*"
  "Leagues buffer name.")

(defvar soccer-teams-buffer "*Soccer Teams*"
  "Teams buffer name.")

(defvar soccer-fixtures-buffer "*Soccer Fixtures*"
  "Fixtures buffer name.")

(defvar soccer-local-cache-path (concat doom-cache-dir (concat "soccer" "/" (int-to-string soccer-season)))
  "Soccer db path prefixed by season.")

(defconst soccer-local-cache-leagues-path (concat soccer-local-cache-path "/leagues")
  "Leagues db path.")

(defconst soccer-local-cache-teams-path (concat soccer-local-cache-path "/teams")
  "Teams db path.")

(defvar soccer-leagues-list-url (concat soccer-base-url "/v3/leagues")
  "Endpoint for all leagues worldwide.")

(defvar soccer-local-cache-leagues-all-path (concat soccer-local-cache-leagues-path
                                                    "/all.data")
  "Absolute path where all leagues are stored.")

(defun soccer-local-cache-teams-all-path (league-id)
  "Determines absolute path where all teams belonging to LEAGUE-ID are stored."
  (concat soccer-local-cache-teams-path
          "/"
          (int-to-string league-id)
          "-all.data"))

(defvar soccer-local-cache-leagues-followed-path (concat soccer-local-cache-leagues-path
                                                         "/followed.data")
  "Absolute path where followed leagues are stored.")

(defvar soccer-local-cache-teams-followed-path (concat soccer-local-cache-teams-path
                                                         "/followed.data")
  "Absolute path where followed teams are stored.")

(defvar soccer-league-name-delimiter " - "
  "Delimiter between league name and league country.")

(defvar soccer-league-download-message
  "No leagues found locally, downloading will take a while. Would you like to proceed?"
  "Prompt to get user consent for download.")

(defvar soccer-downloaded-leagues-path (concat soccer-local-cache-teams-path
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

(defun soccer-team-fixtures-url (team-id)
  "Return the URL where to fetch the FIXTURES ot the TEAM-ID."
  (concat soccer-base-url (format soccer-team-fixtures-url-template
                                  soccer-season
                                  team-id
                                  soccer-tz
                                  soccer-fixtures-limit)))

(defun soccer-league-filename (league-id)
  "Determine specific league absolute path."
  (concat soccer-local-cache-leagues-path
          "/"
          (int-to-string league-id)
          ".data"))

(defun soccer-team-filename (team-id)
  "Determine specific team absolute path."
  (concat soccer-local-cache-teams-path
          "/"
          (int-to-string team-id)
          ".data"))

;; leagues
;; (defun soccer-parse-league (payload)
;;   "Parse a league PAYLOAD that is an alist of the API response into a soccer league."
;;   (let ((raw-leagues (append (alist-get 'response payload) nil)))
;;     (cl-loop for raw-league in raw-leagues
;;              collect (let* ((league (alist-get 'league raw-league))
;;                             (id (alist-get 'id league))
;;                             (name (alist-get 'name league))
;;                             (type (alist-get 'type league))
;;                             (logo (alist-get 'logo league))
;;                             (country (alist-get 'name (alist-get 'country raw-league))))
;;                        (soccer-league--create :id id
;;                                               :name name
;;                                               :type type
;;                                               :logo logo
;;                                               :country country)))))

;; (defun soccer-fetch-league (league-id)
;;   "Given a LEAGUE-ID integer, retrieves the league from the cache or from remote."
;;   (let* ((league-url (soccer-leagues-url league-id))
;;          (payload    (if-let ((cached (soccer-cache-get league-id
;;                                                         :resource-type 'leagues)))
;;                          cached
;;                        (log "cache miss, consuming request quota")
;;                        (soccer-cache-set league-id
;;                                          (soccer-fetch-url league-url)
;;                                          :resource-type 'leagues))))
;;     (car (soccer-parse-league payload))))

;; (defun soccer-fetch-leagues ()
;;   "Fetch all the predefined leagues."
;;   (cl-loop for league in leagues
;;            collect (soccer-fetch-league (plist-get league :id))))

;; (soccer-fetch-url (soccer-leagues-url 39))
;; (soccer-fetch-league 39)
;; (soccer-fetch-leagues)
;; (soccer-parse-league (soccer-cache-get 4 :resource-type 'leagues))

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
            (path (soccer-local-cache-teams-all-path league-id))
            (cached (file-exists-p path)))
      (soccer-load-file path)
    (log (format "team cache miss for league %d, remote call made" league-id))
    (let* ((teams  (soccer-parse-teams (soccer-fetch-url url)))
           (pairs (-map (lambda (team)
                                (cons (soccer-team-name team)
                                      team))
                              teams)))
      (soccer--write-to (soccer-local-cache-teams-all-path league-id)
                        pairs)
      (dolist (team teams)
        (soccer--write-to (soccer-team-filename (soccer-team-id team))
                          (cons (soccer-team-name team)
                                team)))
      pairs)))

;; (soccer-fetch-teams 314)

(defun soccer-fetch-fixtures (team-id)
  "Fetch upcoming fixtures of TEAM-ID.
No caching is applied on fixtures since they are dynamic and change on a daily or even hourly basis.
We want to always see the latest and greatest in fixture details."
  (let* ((url (soccer-team-fixtures-url team-id))
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

;; (cl-defun soccer-cache-get (id &key resource-type)
;;   "Return cache result for RESOURCE-TYPE.

;; RESOURCE-TYPE can be one of 'leagues or 'teams.
;; ID must be an integer that identifies the resource when it was stored in the database.
;; The returned value is the original response alist as it was written when the API call was made."
;;   (let* ((sources (soccer-cache-sources))
;;         (stored-leagues (alist-get 'leagues sources))
;;         (stored-teams (alist-get 'teams sources)))
;;     (pcase resource-type
;;       ('leagues
;;        (when-let ((raw  (alist-get id stored-leagues)))
;;          (soccer-load-file raw)))
;;       ('teams
;;        (when-let ((raw (alist-get id stored-teams)))
;;          (soccer-load-file raw))))))

;; (cl-defun soccer-cache-set (id payload &key resource-type)
;;   "Set cache for RESOURCE-TYPE with ID and returns the cached PAYLOAD.

;; RESOURCE-TYPE can be one of 'leagues or 'teams.
;; ID must be an integer that uniquely identifies the resource being written.
;; The PAYLOAD is returned after the write is completed."
;;   (pcase resource-type
;;     ('leagues
;;      (soccer--write-to (concat soccer-local-cache-leagues-path
;;                                "/"
;;                                (number-to-string id)
;;                                ".data")
;;                        payload)
;;      payload)
;;     ('teams
;;      (soccer--write-to (concat soccer-local-cache-teams-path
;;                                "/"
;;                                (number-to-string id)
;;                                ".data")
;;                        payload)
;;      payload)))

(defun soccer--write-to (filepath content)
  "Write CONTENT to FILEPATH.

FILEPATH is the absolute path of the destination file.
CONTENT can be any elisp object."
  (with-temp-buffer
    (insert (format "%S" content))
    (write-file filepath)))

(defun soccer-cache-source (dirpath)
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

(defun soccer-cache-sources ()
  "Return list of all (FILENAME . FILEPATH) in database.

fILEPATH contains a soccer endpoint API response.
FILENAME is a symbol representing the file.
It creates the base databse directory if it doesn't exist alongside its subdirectories."
  ;; (unless  (file-directory-p soccer-local-cache-path)
  ;;   (mkdir soccer-local-cache-path)
  ;;   (mkdir soccer-local-cache-leagues-path)
  ;;   (mkdir soccer-local-cache-teams-path))
  (let* ((leagues-raw (soccer-cache-source soccer-local-cache-leagues-path))
         (teams-raw (soccer-cache-source soccer-local-cache-teams-path)))
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
      (with-current-buffer (get-buffer-create "*api-call*")
        (insert (format soccer-api-log-template url))
        (insert-buffer response-buffer)
        (newline)))
    (with-current-buffer response-buffer
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun log (content)
  "Log string content to buffer specified by `soccer-log' when `soccer-debug' is non nil."
  (when soccer-debug
    (with-current-buffer (get-buffer-create soccer-log nil)
      (insert content)
      (newline))))

;; (defun soccer--known-teams ()
;;   "Return a list of known teams.
;; Known teams are teams present in the local database
;; which means that their leagues have been accessed at some point."
;;   (let* ((sources (soccer-cache-sources))
;;          (raw-teams (alist-get 'teams sources))
;;          (teams (car-safe (cl-loop for raw-team in raw-teams
;;                        collect (cl-destructuring-bind (id . filename) raw-team
;;                                  (soccer-parse-teams (soccer-load-file filename)))))))
;;     (cl-loop for team in teams
;;              collect (cons (soccer-team-name team)
;;                            (soccer-team-id team)))))

(defun soccer-leagues-visit-teams (button)
  "Action when a league is selected from the table.
Show all the teams that play in that league for that season."
  (let ((league (button-get button 'league)))
    (list-soccer-teams (soccer-league-id league))))

(defun soccer-leagues--refresh ()
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

;; (soccer-leagues--refresh)
;; (soccer-fetch-leagues)

;;;###autoload
(define-derived-mode soccer-league-mode tabulated-list-mode "SoccerLeague"
  "Mode to view soccer leagues.
Right now, the leagues that are viewed are predefined selection that is hardcoded.
They represent the three most popular soccer leagues in the world.
Popularity is assessed based on reported viewership."
  (setq tabulated-list-format (vector '("Name" 15 t)
                                      '("Type" 15 t)
                                      '("Country" 0 t)))
  (add-hook 'tabulated-list-revert-hook 'soccer-leagues--refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun list-soccer-leagues (&optional buff)
  "Entry function to explore soccer teams and upcoming fixtures."
  (interactive)
  (unless buff
    (setq buff (get-buffer-create soccer-leagues-buffer)))
  (with-current-buffer soccer-leagues-buffer
    (soccer-league-mode)
    (soccer-leagues--refresh)
    (tabulated-list-print))
  (display-buffer soccer-leagues-buffer))

(defun soccer-teams-visit-fixtures (button)
  "Action when a team is selected from the table.

Show all upcoming fixtures for that team.
The number of fixtures is capped by `soccer-fixtures-limit'."
  (let ((team (button-get button 'team)))
    (list-soccer-fixtures (soccer-team-id team))))

(defun soccer-teams--refresh (league-id)
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
                               (soccer-team-code team)
                               (soccer-team-country team)
                               (int-to-string (soccer-team-founded team))))
            tabulated-list-entries))))

;;;###autoload
(define-derived-mode soccer-team-mode tabulated-list-mode "SoccerTeam"
  "Mode to view teams within a league."
  (setq tabulated-list-format (vector '("Name" 25 t)
                                      '("Code" 15 t)
                                      '("Country" 15 t)
                                      '("Founded" 0 t)))
  ;; (add-hook 'tabulated-list-revert-hook 'soccer-teams--refresh nil t)
  (tabulated-list-init-header))

;;;autoload
(defun list-soccer-teams (league-id &optional buff)
  "Entry point to list all soccer teams playing in LEAGUE-ID.
It reads the desired league from the minibuffer if none is provided."
  (interactive (list (let ((league-name (completing-read "League: "
                                                         (mapcar #'car
                                                                 (soccer--fetch-leagues-all)))))
                       (soccer-league-id (cdr (assoc league-name (soccer--fetch-leagues-all)))))))
  (unless buff
    (setq buff (get-buffer-create soccer-teams-buffer)))
  (with-current-buffer soccer-teams-buffer
    (soccer-team-mode)
    (soccer-teams--refresh league-id)
    (tabulated-list-print))
  (display-buffer soccer-teams-buffer))

(defun soccer-fixtures--refresh (team-id)
  "Populate table entries with upcoming fixtures for team identified by TEAM-ID.
TEAM-ID are not known in advance, they are retrieved from the leagues.
The fixtures are sorted in ascending order of schedule."
  (setq tabulated-list-entries nil)
  (let ((fixtures (-sort (lambda (x y)
                           (>  (soccer-fixture-timestamp x)
                               (soccer-fixture-timestamp y)))
                         (soccer-fetch-fixtures team-id))))
    (dolist (fixture fixtures)
      (push (list fixture (vector (s-truncate 15 (soccer-fixture-league fixture))
                                  (format-time-string "%a, %b %d" (soccer-fixture-timestamp fixture))
                                  (format-time-string "%I:%M %p" (soccer-fixture-timestamp fixture))
                                  (s-truncate 12 (soccer-fixture-venue fixture))
                                  (s-truncate 12 (soccer-fixture-home fixture))
                                  (s-truncate 12 (soccer-fixture-away fixture))
                                  (s-truncate 17 (soccer-fixture-status fixture))
                                  (soccer-fixture-round fixture)))
            tabulated-list-entries))))

;; (soccer-fetch-fixtures 39)
;; (soccer-fixtures--refresh 39)

;;;###autoload
(define-derived-mode soccer-fixture-mode tabulated-list-mode "SoccerFixture"
  "Mode to view upcoming fixtures of a team."
  (setq tabulated-list-format (vector '("League" 15 t)
                                      '("Date" 15 t)
                                      '("Time" 10 t)
                                      '("Venue" 15 t)
                                      '("Home" 15 t)
                                      '("Away" 15 t)
                                      '("Status" 20 t)
                                      '("Round" 0 t)))
  ;; (add-hook 'tabulated-list-revert-hook 'soccer-teams--refresh nil t)
  ;; (setq tabulated-list-sort-key (cons "Time" t))
  (tabulated-list-init-header))

;;;###autoload
(defun list-soccer-fixtures (team-id &optional buff)
  "Entry point to list upcoming fixtures of a team identified by TEAM-ID."
  (interactive (list (let* ((teams (soccer--fetch-teams-all))
                            (team-name  (completing-read "Team: "
                                                         (-map #'car teams))))
                       (cdr (assoc team-name teams)))))
  (unless buff
    (setq buff (get-buffer-create soccer-fixtures-buffer)))
  (with-current-buffer soccer-fixtures-buffer
    (soccer-fixture-mode)
    (soccer-fixtures--refresh team-id)
    (tabulated-list-print))
  (display-buffer soccer-fixtures-buffer))

;; (list-soccer-fixtures 798)

(defun soccer-downloaded-leagues-p ()
  "Determines whether leagues have been downloaded locally."
  (soccer-cache-source soccer-local-cache-leagues-path))

(defun soccer--fetch-leagues-all ()
  "Fetch and store all leagues worldwide.
If leagues are present in the local database, return it.
Otherwise retrieve leagues from remote, store them locally and return it.
Leagues are stored as an alist of (LEAGUE-NAME . SOCCER-LEAGUE) in
individual files in `soccer-local-cache-leagues-path'."
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
      (log "No leagues in datastore, fetch from remote and store.")
      (soccer--write-to soccer-local-cache-leagues-all-path
                        pairs)
      (dolist (pair pairs)
        (let* ((league (cdr pair))
               (league-id (soccer-league-id league)))
          (soccer--write-to (soccer-league-filename league-id)
                            pair)))))
  (let* ((raw-leagues (soccer-load-file soccer-local-cache-leagues-all-path)))
    raw-leagues))

(defun soccer--fetch-teams-all ()
  "Fetch and store all teams of `followed-leagues' for the given season.
If league's teams is already present, return them.
Otherwise retrieve teams from remote, store them and return it.
Team are stored as an alist of (TEAM-NAME . SOCCER-TEAM) in
individual files in `soccer-local-cache-teams-path'."
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

;; (soccer-fetch-teams 714)
;; (soccer--fetch-teams-all)

(defun soccer--followed-leagues ()
  "If there are followed leagues, return alist of (LEAGUE-NAME . SOCCER-LEAGUE).
Load alist from local database at `soccer-local-cache-leagues-path/followed.data'."
  (let* ((followed (soccer-load-file soccer-local-cache-leagues-followed-path)))
    followed))

(defun soccer--followed-teams ()
  "If there are followed teams, return alist of (TEAM-NAME . SOCCER-TEAM).
Load alist from local database at `soccer-local-cache-leagues-path/followed.data'."
  (let* ((followed (soccer-load-file soccer-local-cache-teams-followed-path)))
    followed))

(defun soccer--fetch-favorite-fixtures (limit)
  "Fetch upcoming fixtures for teams defined in `soccer-followed-teams'.
If non nil, gets up to LIMIT fixtures per team, otherwise gets only one upcoming fixture."
  (setq limit 2)
  (let ((result))
    (dolist (raw-team (soccer--followed-teams) result)
      (let* ((team (cdr raw-team))
             (team-fixtures (soccer-fetch-fixtures (soccer-team-id team)))
             (ordered-fixtures (-sort (lambda (x y)
                                        (> (soccer-fixture-timestamp x)
                                           (soccer-fixture-timestamp y)))
                                      team-fixtures))
             (picked-fixtures (-take limit ordered-fixtures)))
        (push picked-fixtures result)))
    (-sort (lambda (x y)
             (> (soccer-fixture-timestamp x)
                (soccer-fixture-timestamp y)))
           (-flatten result))))

;; (soccer--fetch-favorite-fixtures 2)

(defun soccer--download-and-follow-league (league-name)
  "Interactively select LEAGUE-NAME and tracks it as a `soccer-local-cache-leagues-path/followed.data'.
Teams and upcoming fixtures are derived from `soccer-followed-leagues'."
  (interactive (list (completing-read "Follow league: "
                                      (mapcar #'car (soccer--fetch-leagues-all)))))
  (let* ((all-leagues (soccer--fetch-leagues-all))
         (current-leagues (soccer-load-file soccer-local-cache-leagues-followed-path))
         (indexed-league (assoc league-name all-leagues)))
    (unless (assoc league-name current-leagues)
      (push indexed-league current-leagues)
      (soccer--write-to soccer-local-cache-leagues-followed-path
                        current-leagues))))

(defun soccer-follow-league ()
  "Presents a prompt before proceeding with downloading missing leagues."
  (interactive)
  (if (soccer-downloaded-leagues-p)
      (call-interactively #'soccer--download-and-follow-league)
    (and (yes-or-no-p soccer-league-download-message)
         (call-interactively #'soccer--download-and-follow-league))))

(defun soccer-follow-team (team-name)
  "Interactively select TEAM-NAME to follow and tracks it as `soccer-local-cache-teams-path/followed.data'.
These are considered as favorite teams and their next fixtures can be queried."
  (interactive (list (completing-read "Follow team: "
                                      (mapcar #'car (soccer--fetch-teams-all)))))
  (let* ((all-teams (soccer--fetch-teams-all))
         (current-teams (soccer-load-file soccer-local-cache-teams-followed-path))
         (indexed-team (assoc team-name all-teams)))
    (unless (assoc team-name current-teams)
      (push indexed-team current-teams)
      (soccer--write-to soccer-local-cache-teams-followed-path
                        current-teams))))

(provide 'soccer)
;;; soccer.el ends here
