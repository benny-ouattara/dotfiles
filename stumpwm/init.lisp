#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)

(defvar custom-nord0 "#2e3440")
(defvar custom-nord1 "#3b4252")
(defvar custom-nord2 "#434c5e")
(defvar custom-nord3 "#4c566a")
(defvar custom-nord4 "#d8dee9")
(defvar custom-nord5 "#e5e9f0")
(defvar custom-nord6 "#eceff4")
(defvar custom-nord7 "#8fbcbb")
(defvar custom-nord8 "#88c0d0")
(defvar custom-nord9 "#81a1c1")
(defvar custom-nord10 "#5e81ac")
(defvar custom-nord11 "#bf616a")
(defvar custom-nord12 "#d08770")
(defvar custom-nord13 "#ebcb8b")
(defvar custom-nord14 "#a3be8c")
(defvar custom-nord15 "#b48ead")

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
 stumpwm::*float-window-title-height* 20)
(setq *colors*
      `(,custom-nord1   ;; 0 black
        ,custom-nord11  ;; 1 red
        ,custom-nord14  ;; 2 green
        ,custom-nord13  ;; 3 yellow
        ,custom-nord10  ;; 4 blue
        ,custom-nord14  ;; 5 magenta
        ,custom-nord8   ;; 6 cyan
        ,custom-nord5)) ;; 7 white
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

(defcommand nyxt () ()
            "Run or raise nyxt."
            (run-or-raise "nyxt" '(:class "Nyxt") t nil))

(defcommand qutebrowser () ()
            "Run or raise qutebrowser."
            (run-or-raise "qutebrowser" '(:class "Qutebrowser") t nil))

(defcommand start-emacs () ()
  "Run or raise emacs."
  (run-or-raise "emacs" '(:class "Emacs") t nil))

(defcommand start-slynk (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (slynk:create-server :port (parse-integer port) :dont-close t))
   :name "manual-slynk-stumpwm"))

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

;; (set-border-color "#c792ea")
;; (set-bg-color "#232635")
;; (set-fg-color "#A6Accd")
(set-border-color        custom-nord1)
(set-focus-color         custom-nord1)
(set-unfocus-color       custom-nord3)
(set-float-focus-color   custom-nord1)
(set-float-unfocus-color custom-nord3)
(set-fg-color custom-nord4)
(set-bg-color custom-nord1)
(set-msg-border-width 2)

;; start essential processes
(defparameter *emacs-started* nil)
(unless *emacs-started*
  (progn (run-commands "start-emacs")
         (setf *emacs-started* t)))
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
(mode-line)

(load-module "wifi")
(setf wifi:*use-colors* t
      wifi:*iwconfig-path* "/run/current-system/profile/sbin/iwconfig")
(setf *group-format* "%t")
(setf *window-format* "%n: %30t")
(setf *mode-line-background-color* custom-nord1
      *mode-line-foreground-color* custom-nord5)
(setf *mode-line-border-color* custom-nord1
      *mode-line-border-width* 0)
(setf wifi:*wifi-modeline-fmt*       "%e %P")
(setf *screen-mode-line-format*
      (list
       "[%n]: %w | %B | %l | %I"
       "^>" '(:eval (stumpwm:run-shell-command
                     "LANG=en_US.utf8 date +%A' '%d.%m.%Y' '%l:%M' '%p' 'GMT''%:::z'           '" t))))

;; (load-module "cpu")
;; (load-module "mem")
;; (load-module "screenshot")
;; (load-module "mpd")
(load-module "battery-portable")
(load-module "net")
(load-module "stump-volume-control")

;; system tray
(ql:quickload :xembed)
(load-module "stumptray")
(stumptray::stumptray)
(run-shell-command "nm-applet")
(run-shell-command "volumeicon")

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

;; have this be the last line of config since creating server is not an idempotent operation
(require :slynk)
(slynk:create-server :port 4009 :dont-close t)
