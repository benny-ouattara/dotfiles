#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(setf *default-package* :stumpwm)

(defvar beno-nord0 "#2e3440")
;; (defvar beno-nord1 "#3b4252")
;; (defvar beno-nord2 "#434c5e")
;; (defvar beno-nord3 "#4c566a")
;; (defvar beno-nord4 "#d8dee9")
;; (defvar beno-nord5 "#e5e9f0")
;; (defvar beno-nord6 "#eceff4")
;; (defvar beno-nord7 "#8fbcbb")
;; (defvar beno-nord8 "#88c0d0")
;; (defvar beno-nord9 "#81a1c1")
;; (defvar beno-nord10 "#5e81ac")
;; (defvar beno-nord11 "#bf616a")
;; (defvar beno-nord12 "#d08770")
;; (defvar beno-nord13 "#ebcb8b")
;; (defvar beno-nord14 "#a3be8c")
;; (defvar beno-nord15 "#b48ead")

(set-prefix-key (kbd "C-a"))
(setf
 *resize-increment* 50
 *startup-message* nil
 *mouse-focus-policy* :click
 *message-window-gravity* :center
 *input-window-gravity* :center
 *window-border-style* :thin
 *message-window-padding* 10
 *message-window-y-padding* 10
 *maxsize-border-width* 2
 *normal-border-width* 2
 *transient-border-width* 2
 stumpwm::*float-window-border* 4
 stumpwm::*float-window-title-height* 20
 *debug-level* 10)
;; (setq *colors*
;;       `(,beno-nord1   ;; 0 black
;;         ,beno-nord11  ;; 1 red
;;         ,beno-nord14  ;; 2 green
;;         ,beno-nord13  ;; 3 yellow
;;         ,beno-nord10  ;; 4 blue
;;         ,beno-nord14  ;; 5 magenta
;;         ,beno-nord8   ;; 6 cyan
;;         ,beno-nord5)) ;; 7 white
(redirect-all-output (data-dir-file "debug" "log"))
;; Message bar
(set-fg-color "white")
(set-bg-color "#111111")
(set-border-color "white")
(setf *colors*
      '("#111111"   ; 0 black
        "#dc322f"   ; 1 red
        "#859900"   ; 2 green
        "#b58900"   ; 3 yellow
        "#268bd2"   ; 4 blue
        "#d33682"   ; 5 magenta
        "#2aa198"   ; 6 cyan
        "#fdf6e3")) ; 7 white
(update-color-map (current-screen))

;; set modules path
(set-module-dir "~/.stumpwm.d/modules")

;; define commands
(defcommand now-we-are-six (name age)
  ((:string "Enter your name: ")
   (:number "Enter your age:"))
  (message "~a, in six years you will be ~a" name (+ 6 age)))

(defcommand hsplit-and-focus () ()
  "Create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
            "Create a new frame below and move focus to it."
            (vsplit)
            (move-focus :down))

(defcommand delete-window-and-frame () ()
            "Delete the current frame with its window."
            (delete-window)
            (remove-split))

(defcommand start-nyxt () ()
            "Run or raise nyxt."
            (run-or-raise "nyxt" '(:class "Nyxt") t nil))

(defcommand start-qutebrowser () ()
            "Run or raise qutebrowser."
            (run-or-raise "qutebrowser" '(:class "Qutebrowser") t nil))

(defcommand start-eolie () ()
            "Run or raise eolie web browser."
            (run-or-raise "eolie" '(:class "Eolie") t nil))

(defcommand start-alacritty () ()
  "Run or raise alacritty."
  (run-or-raise "alacritty" '(:class "Alacritty") t nil))

(defcommand start-emacs () ()
  "Run or raise emacs."
  (run-or-raise "emacs" '(:class "Emacs") t nil))

(defcommand start-slynk (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (slynk:create-server :port (parse-integer port) :dont-close t))
   :name "manual-slynk-stumpwm"))

(defcommand start-polybar () ()
  "Run the polybar status bar."
  (run-shell-command "polybar"))

;; enable which-key-mode
(which-key-mode)

;; define workspaces
(defvar *df/workspaces* (list "dev" "web" "term" "random"))
(stumpwm:grename (nth 0 *df/workspaces*))
(dolist (workspace (cdr *df/workspaces*))
  (stumpwm:gnewbg workspace))

(defvar *move-to-keybinds* (list "!" "@" "#" "$" "%" "^" "&" "*" "("))
(dotimes (y (length *df/workspaces*))
  (let ((workspace (write-to-string (+ y 1))))
    (define-key *root-map* (kbd workspace) (concat "gselect " workspace))
    (define-key *root-map* (kbd (nth y *move-to-keybinds*)) (concat "gmove-and-follow " workspace))))

;; define keybindings
(define-key *top-map* (kbd "M-k") "resize-direction Right")
(define-key *top-map* (kbd "M-j") "resize-direction Left")
(define-key *top-map* (kbd "M-l") "resize-direction Up")
(define-key *top-map* (kbd "M-h") "resize-direction Down")

(define-key *top-map* (kbd "s-RET") "exec alacritty")
(define-key *top-map* (kbd "s-w") "qutebrowser")
(define-key *top-map* (kbd "s-e") "emacs")

(define-key *top-map* (kbd "s-j") "move-focus left")
(define-key *top-map* (kbd "s-k") "move-focus right")
(define-key *top-map* (kbd "s-h") "move-focus down")
(define-key *top-map* (kbd "s-l") "move-focus up")

(define-key *top-map* (kbd "s-C-h") "move-window left")
(define-key *top-map* (kbd "s-C-l") "move-window right")
(define-key *top-map* (kbd "s-C-j") "move-window down")
(define-key *top-map* (kbd "s-C-k") "move-window up")

(define-key *top-map* (kbd "s-Q") "quit")
(define-key *top-map* (kbd "s-R") "restart-hard")
(define-key *top-map* (kbd "s-q") "delete")
(define-key *top-map* (kbd "s-r") "remove")

(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-s") "hsplit-and-focus")
(define-key *top-map* (kbd "s-S") "vsplit-and-focus")
(define-key *top-map* (kbd "s-SPC") "run-shell-command dmenu_run -fn 'Droid Sans Mono-17'")
;; (define-key *top-map* (kbd "s-SPC") "run-shell-command rofi -show window")
(define-key *top-map* (kbd "C-s-l") "run-shell-command slock")
(define-key *top-map* (kbd "C-s-r") "iresize")

(define-key *top-map* (kbd "s-1") "gselect dev")
(define-key *top-map* (kbd "s-2") "gselect web")
(define-key *top-map* (kbd "s-3") "gselect term")
(define-key *top-map* (kbd "s-4") "gselect random")
(define-key *top-map* (kbd "s-5") "gselect misc")

(define-key *top-map* (kbd "C-s-1") "gmove dev")
(define-key *top-map* (kbd "C-s-2") "gmove term")
(define-key *top-map* (kbd "C-s-3") "gmove web")
(define-key *top-map* (kbd "C-s-4") "gmove random")
(define-key *top-map* (kbd "C-s-5") "gmove misc")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "backlight-up")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "backlight-down")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "volume-toggle-mute")

;; (set-border-color        beno-nord1)
;; (set-focus-color         beno-nord1)
;; (set-unfocus-color       beno-nord3)
;; (set-float-focus-color   beno-nord1)
;; (set-float-unfocus-color beno-nord3)
;; (set-fg-color beno-nord4)
;; (set-bg-color beno-nord1)
(set-msg-border-width 2)

;; window placement rules
(define-frame-preference "dev"
  (1 t t :class "Emacs"))

(define-frame-preference "web"
  (2 t t :class ".eolie-real"))

(define-frame-preference "web"
  (2 t t :class "Nyxt"))

;; (define-frame-preference "term"
;;     (3 t t :class "Alacritty"))

;; start processes
(stumpwm:gselect "dev")
(run-commands
 "start-polybar"
 "start-nyxt"
 "start-emacs")
(run-shell-command "setxkbmap us -option 'caps:ctrl_modifier'")
(run-shell-command "xcape -e 'Caps_Lock=Escape'")
(run-shell-command "xset r rate 150 60")
(run-shell-command "feh --randomize --bg-fill ~/Sync/wallpapers/*")
(run-shell-command "picom")
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "amixer")

;; backlight
(load-module "acpi-backlight")
(acpi-backlight:init "intel_backlight")

;; gaps
(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 10)
(run-commands "toggle-gaps-on")

(setf *mode-line-timeout* 2)
;; (mode-line)

;; (load-module "wifi")
;; (setf wifi:*use-colors* t
;;       wifi:*iwconfig-path* "/run/current-system/profile/sbin/iwconfig")
;; (setf *group-format* "%t")
;; (setf *window-format* "%n: %30t")
;; (setf *mode-line-background-color* "black"
;;       *mode-line-foreground-color* "white")
;; (setf *mode-line-border-color* "black"
;;       *mode-line-border-width* 0)
;; (setf wifi:*wifi-modeline-fmt*       "%e %P")
;; (setf *screen-mode-line-format*
;;       (list
;;        "[%n]: %w | %B | %l | %I"
;;        "^>" '(:eval (stumpwm:run-shell-command
;;                      "LANG=en_US.utf8 date +%A' '%d.%m.%Y' '%l:%M' '%p' 'GMT''%:::z'           '" t))))

;; Polybar
;; (defun icon-by-group (name)
;;   (cond
;;     ((string-equal name "dev")
;;      "")
;;     ((string-equal name "web")
;;      "")
;;     ((string-equal name "term")
;;      "")
;;     ((string-equal name "Top")
;;      "")
;;     ((string-equal name "Logs")
;;      "")
;;     (t (concat ""))))

;; (defun polybar-groups ()
;;   "Return string representation for polybar stumpgroups module"
;;   (apply #'concatenate 'string
;;          (mapcar
;;           (lambda (g)
;;             (let* ((name (group-name g))
;;                    (number (write-to-string (group-number g)))
;;                    (n-win (write-to-string (length (group-windows g))))
;;                    (icon (icon-by-group name))
;;                    (text (concat " %{F#54728E}" icon "%{F-} " number ":" name " ")))
;;               (cond
;;                 ((eq g (current-group)) (concat "%{F#FFFFFF B#000000 u#54728E +u}" text "[" n-win "] " "%{F- B- u- -u}"))
;;                 ((string-equal n-win "0") "")
;;                 (t (concat "%{F#FFFFFF}" text "[" n-win "] " "%{F-}")))))
;;           (sort (screen-groups (current-screen)) #'< :key #'group-number))))

;; (defun polybar-update-groups ()
;;   (run-shell-command "polybar-msg hook stumpwmgroups 1"))

;; (add-hook *new-window-hook* (lambda (win) (polybar-update-groups)))
;; (add-hook *destroy-window-hook* (lambda (win) (polybar-update-groups)))
;; (add-hook *focus-window-hook* (lambda (win lastw) (polybar-update-groups)))
;; (add-hook *focus-group-hook* (lambda (grp lastg) (polybar-update-groups)))

;; (defun shift-windows-forward (frames win)
;;   (when frames
;;     (let ((frame (car frames)))
;;       (shift-windows-forward (cdr frames)
;;                              (frame-window frame))
;;       (when win
;;         (pull-window win frame)))))

;; (defcommand rotate-windows () ()
;;   "Rotate windows"
;;   (let* ((frames (group-frames (current-group)))
;;          (win (frame-window (car (last frames)))))
;;     (shift-windows-forward frames win)))

;; (defcommand swap-windows () ()
;;   (let* ((group (current-group))
;;          (cur-frame (tile-group-current-frame group))
;;          (frames (group-frames group)))
;;     (if (eq (length frames) 2)
;;         (progn (if (or (neighbour :left cur-frame frames)
;;                        (neighbour :right cur-frame frames))
;;                    (progn
;;                      (only)
;;                      (vsplit))
;;                    (progn
;;                      (only)
;;                      (hsplit))))
;;         (message "Works only with 2 frames"))))

;; ;; Rofi

;; (defun rofi (mode)
;;   (run-shell-command (concat "rofi -show " mode " -m " (write-to-string (head-number (current-head))))))

;; (defcommand rofi-run () ()
;;   (rofi "run -sidebar-mode"))

;; (defcommand rofi-window () ()
;;   (rofi "window"))

;; (defcommand rofi-windowcd () ()
;;   (rofi "windowcd"))

;; (load-module "cpu")
;; (load-module "mem")
;; (load-module "screenshot")
;; (load-module "mpd")
;; (load-module "battery-portable")
;; (load-module "net")
(load-module "stump-volume-control")

;; primitive and unsecure screen lock, prefer slock bound to C-s-l
(load-module "stump-lock")
(setf stump-lock:*password* "asdf")
(define-key *top-map* (kbd "s-l") "lock-screen")

;; password
(load-module "pass")

;; load modules last so that they don't break system in failure case
(ql:quickload :clx-truetype)
(setf xft:*font-dirs* '("/home/ben/.guix-profile/share/fonts/"))
(xft:cache-fonts)
(load-module "ttf-fonts")
(set-font (make-instance 'xft:font :family "JetBrains Mono" :subfamily "Regular" :size 16))
;; (setf clx-truetype:+font-cache-filename+ "/home/ben/.local/share/fonts/font-cache.sexp")

;; system tray
;; (ql:quickload :xembed)
;; (load-module "stumptray")
;; (setf stumptray::*tray-win-background* beno-nord1)
(run-shell-command "nm-applet")
(run-shell-command "volumeicon")
;; (bt:make-thread (lambda ()
;;                   (sleep 10)
;;                   (stumptray::stumptray)))

;; have this be the last line of config since creating server is not an idempotent operation
(require :slynk)
(slynk:create-server :port 4009 :dont-close t)
