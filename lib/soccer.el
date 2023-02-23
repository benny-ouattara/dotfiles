;;; soccer.el Soccer Fixtures -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ben O
;;
;; Author: Ben O <benny.ouattara@gmail.com>
;; Maintainer: Ben O <benny.ouattara@gmail.com>
;; Created: February 18, 2023
;; Modified: February 18, 2023
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/benny-ouattara/soccer
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

(defvar soccer-base-url "https://api-football-v1.p.rapidapi.com")
(defvar soccer-season 2022)
(defvar soccer-tz "America/New_York")
;; NOTE: to get the league id and name, checkout out this endpoint GET /v3/leagues
(defvar leagues '((:name "Premier League" :id 39)
                  (:name "Primera Division" :id 140)
                  (:name "Ligue 1" :id 61)))

;; leagues
(cl-defstruct (soccer-league (:constructor soccer-league--create))
  "Represents a soccer league"
  id name type logo country)

(defun soccer-leagues-url (league-id)
  (concat soccer-base-url (format "/v3/leagues?id=%d&season=%s" league-id soccer-season)))

(defun soccer-parse-league (payload)
  "Parse a league payload into a soccer league."
  (let ((raw-leagues (append (alist-get 'response payload) nil)))
    (cl-loop for raw-league in raw-leagues
             collect (let* ((league (alist-get 'league raw-league))
                            (id (alist-get 'id league))
                            (name (alist-get 'name league))
                            (type (alist-get 'type league))
                            (logo (alist-get 'logo league))
                            (country (alist-get 'name (alist-get 'country raw-league))))
                       (soccer-league--create :id id
                                                :name name
                                                :type type
                                                :logo logo
                                                :country country)))))

(defun soccer-fetch-leagues ()
  (cl-loop for league in leagues
           collect (soccer-fetch-league (plist-get league :id))))

(defun soccer-fetch-league (league-id)
  "Given a LEAGUE-ID integer, retrieves the league either from the cache or from remote."
  (let* ((league-url (soccer-leagues-url league-id))
         (payload (if-let ((cached (soccer-cache-get league-id
                                                       :resource-type 'leagues)))
                      (progn
                        (log "cache hit, returning from cache")
                        cached)
                    (progn
                      (log "cache miss, consuming request quota")
                      (soccer-cache-set league-id
                                               (soccer-fetch-url league-url)
                                               :resource-type 'leagues)))))
    (car (soccer-parse-league payload))))

;; (soccer-fetch-url (soccer-leagues-url 39))
;; (soccer-fetch-league 39)
;; (soccer-fetch-leagues)
;; (soccer-parse-league (soccer-cache-get 4 :resource-type 'leagues))

;; to get all the teams in a league
(cl-defstruct (soccer-team (:constructor soccer-team--create))
  "Represents a soccer team"
  id name code country founded logo)

(defun soccer-teams-url (league-id)
  (concat soccer-base-url (format "/v3/teams?league=%s&season=%d" league-id soccer-season)))

(defun soccer-parse-teams (payload)
  "Parse soccer team from alist API response."
  (cl-loop for raw-team in  (append (alist-get 'response payload) nil)
           collect (let* ((team (alist-get 'team raw-team)))
                     (soccer-team--create :id (alist-get 'id team)
                                            :name (alist-get 'name team)
                                            :code (alist-get 'code team)
                                            :country (alist-get 'country team)
                                            :founded (alist-get 'founded team)
                                            :logo (alist-get 'logo team)))))

(defun soccer-fetch-teams (league-id)
  "Fetch teams in a LEAGUE-ID."
  (let* ((url (soccer-teams-url league-id)))
    (if-let* ((cached (soccer-cache-get league-id
                                          :resource-type 'teams)))
        (progn
          (log (format  "team cache hit for league %d, no remote call" league-id))
          (soccer-parse-teams cached))
      (progn
        (log (format "team cache miss for league %d, remote call made" league-id))
        (soccer-parse-teams (soccer-cache-set league-id
                                                  (soccer-fetch-url url)
                                                  :resource-type 'teams))))))

;; fixtures
(defvar soccer-fixtures-limit 25)
(defvar soccer-team-fixtures-url-template "/v3/fixtures?season=%s&team=%d&timezone=%s&next=%d")

(cl-defstruct (soccer-fixture (:constructor soccer-fixture--create))
  timestamp venue status league round home away)

(defun soccer-team-fixtures-url (team-id)
  (concat soccer-base-url (format soccer-team-fixtures-url-template soccer-season team-id soccer-tz soccer-fixtures-limit)))

(defun soccer-fetch-fixtures (team-id)
  "Fetch upcoming fixtures for TEAM-ID."
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

;; store
(defvar soccer-local-cache-path (concat doom-cache-dir "soccer"))
(defconst soccer-local-cache-leagues-path (concat soccer-local-cache-path "/leagues"))
(defconst soccer-local-cache-teams-path (concat soccer-local-cache-path "/teams"))

(cl-defun soccer-cache-get (id &key resource-type)
  "Return cache result for RESOURCE-TYPE.
RESOURCE-TYPE can be one of 'leagues or 'teams. ID is an integer.
Result is the raw alist as it is written to cache files. This function will not handle parsing."
  (let ((sources (soccer-cache-sources)))
    (pcase resource-type
      ('leagues
       (if-let ((raw  (alist-get id (alist-get 'leagues sources))))
           (soccer-load-file raw)))
      ('teams
       (if-let ((raw (alist-get id (alist-get 'teams sources))))
           (soccer-load-file raw))))))

(cl-defun soccer-cache-set (id payload &key resource-type)
  "Set cache for RESOURCE-TYPE with ID and returns the cached PAYLOAD.
RESOURCE-TYPE can be one of 'league or 'team. ID is an integer representing the resource id to write.
Return PAYLOAD after write is complete."
  (pcase resource-type
    ('leagues
     (write-to (concat soccer-local-cache-leagues-path "/" (number-to-string id) ".data") payload)
     payload)
    ('teams
     (write-to (concat soccer-local-cache-teams-path "/" (number-to-string id) ".data") payload)
     payload)))

(defun write-to (filepath content)
  "Write CONTENT to FILEPATH.
FILEPATH is the absolute path of the destination file. CONTENT can be any object."
  (with-temp-buffer
    (insert (format "%S" content))
    (write-file filepath)))

(defun soccer-cache-source (filepath)
  "Return cache payload at FILEPATH."
  (mapcar (lambda (filepath)
            (cons (string-to-number (downcase (file-name-base filepath)))
                  filepath))
          (f-files filepath (lambda (filename)
                              (s-ends-with? ".data" filename)))))

(defun soccer-cache-sources ()
  "Return list of cached (filename . filepath).
Filepath contains the an API response.
Filename is a symbol representing the file."
  (unless  (file-directory-p soccer-local-cache-path)
    (mkdir soccer-local-cache-path)
    (mkdir soccer-local-cache-leagues-path)
    (mkdir soccer-local-cache-teams-path))
  (let* ((leagues-raw (soccer-cache-source soccer-local-cache-leagues-path))
         (teams-raw (soccer-cache-source soccer-local-cache-teams-path)))
    `((leagues . ,leagues-raw) (teams . ,teams-raw))))

(defun soccer-load-file (filename)
  "Return alist content at FILENAME."
  (let ((raw (with-current-buffer (find-file-noselect filename)
               (goto-char (point-min))
               (read (buffer-string)))))
    (append raw nil)))

;; client
(defun soccer-fetch-url (url)
  "Fetch url content.
Assumes that response is json formatted, tries to read body with (json-read)"
  (let* ((url-request-method "GET")
        (url-user-agent (format "%s Agent" user-full-name))
        ;; TODO: obfuscate the API key in authsecrets
        (url-request-extra-headers '(("X-Rapidapi-Host" . "api-football-v1.p.rapidapi.com")))
        (response-buffer (url-retrieve-synchronously url 'silent 'inhibit-cookies)))
    (with-current-buffer (get-buffer-create "*api-call*")
      (insert (format "===================API CALL: %s===================" url))
      (insert-buffer response-buffer)
      (newline))
    (with-current-buffer response-buffer
      (goto-char url-http-end-of-headers)
      (json-read))))

;; logging

(defvar soccer-log "*soccer-log*")

(defun log (content)
  "Log string content to *soccer-log* buffer."
  (with-current-buffer (get-buffer-create soccer-log nil)
    (insert content)
    (newline)))

;; mode

(defun soccer-leagues-visit-teams (button)
  (let ((league (button-get button 'league)))
    (list-soccer-teams (soccer-league-id league))))

(defun soccer-leagues--refresh ()
  (setq tabulated-list-entries nil)
  (let ((leagues (soccer-fetch-leagues)))
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

(define-derived-mode soccer-league-mode tabulated-list-mode "SoccerLeague"
  "Mode to view different soccer leagues."
  (setq tabulated-list-format (vector '("Name" 15 t)
                                      '("Type" 15 t)
                                      '("Country" 0 t)))
  (add-hook 'tabulated-list-revert-hook 'soccer-leagues--refresh nil t)
  (tabulated-list-init-header))

(defvar soccer-leagues-buffer "*Soccer Leagues*")
(defvar soccer-teams-buffer "*Soccer Teams*")
(defvar soccer-fixtures-buffer "*Soccer Fixtures*")

(defun list-soccer-leagues (&optional buff)
  (interactive)
  (unless buff
    (setq buff (get-buffer-create soccer-leagues-buffer)))
  (with-current-buffer soccer-leagues-buffer
    (soccer-league-mode)
    (soccer-leagues--refresh)
    (tabulated-list-print))
  (display-buffer soccer-leagues-buffer))

(defun soccer-teams-visit-fixtures (button)
  (let ((team (button-get button 'team)))
    (message "visiting %S" (soccer-team-id team))
    (list-soccer-fixtures (soccer-team-id team))))

(defun soccer-teams--refresh (league-id)
  (setq tabulated-list-entries nil)
  (let ((teams (soccer-fetch-teams league-id)))
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

(define-derived-mode soccer-team-mode tabulated-list-mode "SoccerTeam"
  "Mode to view teams within a league."
  (setq tabulated-list-format (vector '("Name" 15 t)
                                      '("Code" 15 t)
                                      '("Country" 15 t)
                                      '("Founded" 0 t)))
  ;; (add-hook 'tabulated-list-revert-hook 'soccer-teams--refresh nil t)
  (tabulated-list-init-header))

(defun list-soccer-teams (league-id &optional buff)
  (interactive (list (let ((league-name
                            (completing-read "League: "
                                             (mapcar (lambda (p)
                                                       (plist-get p :name))
                                                     leagues))))
                       (plist-get  (car (-filter (lambda (p)
                                                   (equal league-name (plist-get p :name)))
                                                 leagues))
                                   :id))))
  (unless buff
    (setq buff (get-buffer-create soccer-teams-buffer)))
  (with-current-buffer soccer-teams-buffer
    (soccer-team-mode)
    (soccer-teams--refresh league-id)
    (tabulated-list-print))
  (display-buffer soccer-teams-buffer))

;; fixtures mode
;; timestamp venue status league round home away
(defun soccer-fixtures--refresh (team-id)
  (setq tabulated-list-entries nil)
  (let ((fixtures (-sort (lambda (x y)
                           (>  (soccer-fixture-timestamp x)
                               (soccer-fixture-timestamp y)))
                         (soccer-fetch-fixtures team-id))))
    (dolist (fixture fixtures)
      (push (list fixture (vector (soccer-fixture-league fixture)
                                  (format-time-string "%a, %b %d" (soccer-fixture-timestamp fixture))
                                  (format-time-string "%I:%M %p" (soccer-fixture-timestamp fixture))
                                  (soccer-fixture-venue fixture)
                                  (soccer-fixture-home fixture)
                                  (soccer-fixture-away fixture)
                                  (soccer-fixture-status fixture)
                                  (soccer-fixture-round fixture)))
            tabulated-list-entries))))

;; (soccer-fixtures--refresh 39)

(define-derived-mode soccer-fixture-mode tabulated-list-mode "SoccerFixture"
  "Mode to view a team's fixtures."
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

(defun list-soccer-fixtures (team-id &optional buff)
  (unless buff
    (setq buff (get-buffer-create soccer-fixtures-buffer)))
  (with-current-buffer soccer-fixtures-buffer
    (soccer-fixture-mode)
    (soccer-fixtures--refresh team-id)
    (tabulated-list-print))
  (display-buffer soccer-fixtures-buffer))

;; (list-soccer-fixtures 798)

;; id name code country founded logo
(provide 'soccer)
;;; soccer.el ends here
