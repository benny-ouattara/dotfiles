#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; Move prefix key out of the way - all bindings are on Super directly
(set-prefix-key (kbd "C-F20"))
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
 ;; Level 10 traces every X event (the log reached hundreds of MB)
 *debug-level* 1)
;; Start a fresh debug log once the old one passes 10 MB
(let ((log (data-dir-file "debug" "log")))
  (when (and (probe-file log)
             (> (with-open-file (in log) (file-length in)) (* 10 1024 1024)))
    (delete-file log)))
(redirect-all-output (data-dir-file "debug" "log"))

;; Lisp systems from the Guix home profile (swm-gaps, clx-truetype, ...)
(asdf:initialize-source-registry
 '(:source-registry
   (:include "/home/ben/.guix-home/profile/etc/common-lisp/source-registry.conf.d/")
   :inherit-configuration))

;; Set DBUS_SESSION_BUS_ADDRESS for nix apps (jeepney can't handle autolaunch:)
;; Done before any process is started so they all inherit it.
(let ((addr (string-trim '(#\Newline #\Space)
                         (run-shell-command
                          "ss -xlp 2>/dev/null | grep dbus-daemon | grep -oP '/tmp/dbus-\\S+' | head -1 | xargs -I{} echo 'unix:path={}'" t))))
  (when (> (length addr) (length "unix:path="))
    (sb-posix:setenv "DBUS_SESSION_BUS_ADDRESS" addr 1)))

;; Default theme colors; guix/scripts/theme-switch writes the current theme's
;; values to ~/.config/theme/current/stumpwm.lisp, loaded below
(defparameter *theme-fg* "#CDD6F4")
(defparameter *theme-bg* "#1E1E2E")
(defparameter *theme-bg-alt* "#313244")
(defparameter *theme-red* "#F38BA8")
(defparameter *theme-green* "#A6E3A1")
(defparameter *theme-yellow* "#F9E2AF")
(defparameter *theme-blue* "#89B4FA")
(defparameter *theme-purple* "#CBA6F7")
(defparameter *theme-cyan* "#94E2D5")
(defparameter *theme-muted* "#585B70")
(defparameter *theme-font* "Iosevka Term")

(defun apply-theme-colors ()
  "Apply the *theme-...* colors to the message bar and ^0-^7 color codes."
  (set-fg-color *theme-fg*)
  (set-bg-color *theme-bg*)
  (set-border-color *theme-blue*)
  (setf *colors* (list *theme-bg* *theme-red* *theme-green* *theme-yellow*
                       *theme-blue* *theme-purple* *theme-cyan* *theme-fg*))
  (update-color-map (current-screen)))

;; Regenerate the theme files (outside git) so polybar, rofi, kitty and dunst
;; find them, then take StumpWM's colors from the current theme
(run-shell-command "/home/ben/Code/dotfiles/guix/scripts/theme-switch --refresh" t)
(load (merge-pathnames ".config/theme/current/stumpwm.lisp" (user-homedir-pathname))
      :if-does-not-exist nil)
(apply-theme-colors)

;; define commands
(defcommand hsplit-and-focus () ()
  "Create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "Create a new frame below and move focus to it."
  (vsplit)
  (move-focus :down))

(defun emacs-window-p (win)
  "True when WIN is an Emacs window. WIN is nil in an empty frame."
  (and win (string-equal (window-class win) "Emacs")))

(defcommand unified-copy () ()
  "Copy: in Emacs send M-w, otherwise promote X PRIMARY selection to CLIPBOARD."
  (let ((win (current-window)))
    (if (emacs-window-p win)
        (send-fake-key win (kbd "M-w"))
        (run-shell-command "xclip -selection primary -o | xclip -selection clipboard -i"))))

(defcommand unified-cut () ()
  "Cut: send C-w to Emacs, copy PRIMARY to CLIPBOARD for everything else."
  (let ((win (current-window)))
    (if (emacs-window-p win)
        (send-fake-key win (kbd "C-w"))
        (run-shell-command "xclip -selection primary -o | xclip -selection clipboard -i"))))

(defun super-held-p ()
  "True while a Super key is physically down."
  (let ((mask (nth-value 4 (xlib:query-pointer (screen-root (current-screen))))))
    (intersection (modifiers-super *modifiers*) (xlib:make-state-keys mask))))

(defun type-clipboard (&optional (tries 50))
  "Type CLIPBOARD once Super is released, polling every 20ms for about a second.
Typing while Super is held would fire s- bindings, and xdotool's
--clearmodifiers re-presses Super afterwards, leaving it stuck (Enter
then runs s-RET). Command substitution drops trailing newlines."
  (cond ((not (super-held-p))
         (run-shell-command "xdotool type -- \"$(xclip -selection clipboard -o)\""))
        ((plusp tries)
         (run-with-timer 0.02 nil #'type-clipboard (1- tries)))
        (t (message "Paste skipped: Super still held"))))

(defcommand unified-paste () ()
  "Paste: send C-y to Emacs, type CLIPBOARD contents for everything else."
  (let ((win (current-window)))
    (if (emacs-window-p win)
        (send-fake-key win (kbd "C-y"))
        (type-clipboard))))

(defcommand clipboard-history () ()
  "Show clipboard history via clipmenu with rofi."
  (run-shell-command "CM_LAUNCHER=rofi clipmenu -theme ~/.config/rofi/menu.rasi"))

(defcommand cycle-wallpaper () ()
  "Next wallpaper for the current theme."
  (run-shell-command "/home/ben/Code/dotfiles/guix/scripts/wallpaper next"))

(defcommand toggle-float () ()
  "Toggle the current window between floating and tiled."
  (let ((win (current-window)))
    (when win
      (if (typep win 'float-window)
          (unfloat-this)
          (float-this)))))

(defcommand audio-switch () ()
  "Switch audio sink via rofi."
  (run-shell-command
   (concatenate 'string
                "sink=$(pactl list sinks | grep -E 'Name:|Description:' | paste - - | "
                "sed 's/.*Name: //;s/\\t.*Description: / ➜ /' | "
                "rofi -dmenu -p 'Audio' -theme ~/.config/rofi/menu.rasi | "
                "cut -d' ' -f1); "
                "[ -n \"$sink\" ] && pactl set-default-sink \"$sink\" && "
                "notify-send -h string:x-dunst-stack-tag:audio 'Audio Output' \"$(pactl list sinks | grep -A1 \"$sink\" | grep Description | sed 's/.*: //')\"")))

(defcommand volume-adjust (delta) ((:string "Volume change (e.g. +5%): "))
  "Change the default sink volume by DELTA and show a notification."
  (run-shell-command
   (format nil "pactl set-sink-volume @DEFAULT_SINK@ ~a && notify-send -h string:x-dunst-stack-tag:volume \"Volume\" \"$(pactl get-sink-volume @DEFAULT_SINK@ | grep -oP '\\d+%' | head -1)\""
           delta)))

(defcommand vol-up () ()
  "Raise volume and show notification."
  (volume-adjust "+5%"))

(defcommand vol-down () ()
  "Lower volume and show notification."
  (volume-adjust "-5%"))

(defcommand vol-toggle () ()
  "Toggle mute and show notification."
  (run-shell-command "pactl set-sink-mute @DEFAULT_SINK@ toggle && notify-send -h string:x-dunst-stack-tag:volume \"Volume\" \"$(pactl get-sink-mute @DEFAULT_SINK@ | cut -d: -f2)\""))

(defcommand mic-toggle () ()
  "Toggle microphone mute and show notification."
  (run-shell-command "pactl set-source-mute @DEFAULT_SOURCE@ toggle && notify-send -h string:x-dunst-stack-tag:mic \"Microphone\" \"$(pactl get-source-mute @DEFAULT_SOURCE@ | cut -d: -f2)\""))

(defcommand bright-up () ()
  "Raise brightness and show notification."
  (run-shell-command "brightnessctl set +10% && notify-send -h string:x-dunst-stack-tag:brightness \"Brightness\" \"$(brightnessctl -m | cut -d, -f4)\""))

(defcommand bright-down () ()
  "Lower brightness and show notification."
  (run-shell-command "brightnessctl set 10%- && notify-send -h string:x-dunst-stack-tag:brightness \"Brightness\" \"$(brightnessctl -m | cut -d, -f4)\""))

(defcommand notifications-toggle-silence () ()
  "Pause or resume dunst notifications."
  (run-shell-command "dunstctl set-paused toggle" t)
  ;; Refresh the do-not-disturb indicator on the bar
  (run-shell-command "polybar-msg action dnd hook 0")
  (message "Notifications ~a"
           (if (search "true" (run-shell-command "dunstctl is-paused" t))
               "silenced"
               "on")))

(defcommand screenshot-screen () ()
  "Take a fullscreen screenshot, save it and copy it to the clipboard."
  (run-shell-command "f=~/Screenshots/$(date +%Y%m%d-%H%M%S).png; maim \"$f\" && xclip -selection clipboard -t image/png -i \"$f\" && notify-send 'Screenshot saved' \"$f\""))

(defcommand screenrecord-region () ()
  "Record a screen region with guix/scripts/record; press again to stop."
  (run-shell-command "/home/ben/Code/dotfiles/guix/scripts/record region"))

(defcommand screenrecord-screen () ()
  "Record the screen with desktop audio; press again to stop."
  (run-shell-command "/home/ben/Code/dotfiles/guix/scripts/record screen desktop"))

(defcommand screenshot-region () ()
  "Screenshot a selected region, save it and copy it to the clipboard."
  (run-shell-command "f=~/Screenshots/$(date +%Y%m%d-%H%M%S).png; maim -s \"$f\" && xclip -selection clipboard -t image/png -i \"$f\" && notify-send 'Screenshot saved' \"$f\""))

(defun binding-description (command)
  "Readable text for a key binding's COMMAND string: `Run ...' for exec, the
first docstring line for commands without arguments, else the command itself."
  (let* ((space (position #\Space command))
         (name (subseq command 0 space))
         (symbol (find-symbol (string-upcase name) :stumpwm))
         (doc (and symbol (fboundp symbol) (documentation symbol 'function))))
    (cond ((string= name "exec") (concat "Run " (subseq command (1+ space))))
          ((and doc (not space)) (subseq doc 0 (position #\Newline doc)))
          (t command))))

(defcommand show-keybindings () ()
  "Show the key bindings, generated from *top-map*."
  (let ((file (merge-pathnames ".cache/stumpwm/keys.txt" (user-homedir-pathname))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      (dolist (binding (kmap-bindings *top-map*))
        (let ((command (binding-command binding)))
          (when (stringp command)
            (format out "~22a ~a~%" (print-key (binding-key binding))
                    (binding-description command))))))
    (run-shell-command
     (format nil "rofi -dmenu -i -no-custom -p Keys -theme ~~/.config/rofi/menu.rasi -theme-str 'window {width: 1000px;}' < ~a"
             (namestring file)))))

(defcommand start-slynk (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (slynk:create-server :port (parse-integer port) :dont-close t))
   :name "manual-slynk-stumpwm"))

(defun forget-dock-mode-lines ()
  "Drop StumpWM's mode-line entries for dock windows (they have no gcontext).
A restarted polybar reuses its X window id, so the old bar's entry keeps the
head and StumpWM never maps the new bar."
  (setf *mode-lines* (remove-if-not #'mode-line-cc *mode-lines*)))

(defcommand start-polybar () ()
  "Kill existing polybar, start fresh, and fill the groups module once it is up."
  (run-shell-command
   (concat "polybar-msg cmd quit 2>/dev/null; "
           "while pgrep -x polybar >/dev/null; do sleep 0.1; done; "
           "stumpish eval '(forget-dock-mode-lines)' >/dev/null; "
           "polybar --config=/home/ben/Code/dotfiles/polybar/tokyo/config.ini main &"))
  (run-with-timer 2 nil 'polybar-update-groups))

(defun rofi (mode)
  (run-shell-command (concat "rofi -show " mode " -m " (write-to-string (head-number (current-head))) " -theme ~/.config/rofi/launcher.rasi")))

(defcommand launch-rofi () ()
  "Launch an application with rofi."
  (rofi "drun"))

(defcommand rofi-window () ()
  "Switch to a window with rofi."
  (rofi "window"))

(defcommand menu (&optional section) (:rest)
  "Open the system menu (guix/scripts/menu), or one SECTION of it."
  (run-shell-command
   (format nil "/home/ben/Code/dotfiles/guix/scripts/menu ~@[~a~]" section)))

(defun guix-run (cmd)
  (gselect "sys")
  (run-shell-command cmd))

(defcommand guix-up (target) ((:string "up target: "))
  "Run an `up' (guix/Makefile) target in a terminal on sys, notifying when done."
  (guix-run
   (format nil "exec kitty zsh -c 'up ~a && notify-send Guix \"~:*~a finished\" || notify-send -u critical Guix \"~:*~a failed\"; exec zsh'"
           target)))

(defcommand guix-system () ()
  "Reconfigure guix system."
  (guix-up "system-reconfigure"))

(defcommand guix-home () ()
  "Reconfigure guix home."
  (guix-up "home-reconfigure"))

(defparameter *config-files*
  '(("home" . "guix/home/config.scm")
    ("system" . "guix/system/config.scm")
    ("stumpwm" . "stumpwm/init.lisp")
    ("polybar" . "polybar/tokyo/modules.ini")
    ("rofi" . "rofi/launcher.rasi")
    ("kitty" . "kitty/kitty.conf")
    ("theme" . "guix/scripts/theme-switch"))
  "Configs `edit-config' opens, relative to the dotfiles checkout.")

(defcommand edit-config (name) ((:string "Config: "))
  "Open a dotfiles config (see *config-files*) in terminal Emacs."
  (let ((file (cdr (assoc name *config-files* :test #'string-equal))))
    (if file
        (run-shell-command
         (format nil "exec kitty --directory=/home/ben/Code/dotfiles emacsclient -t ~a" file))
        (message "No config named ~a" name))))

;; enable which-key-mode
(which-key-mode)

;; define workspaces
(defvar *df/workspaces* (list "dev" "web" "term" "mail" "sys"))
(stumpwm:grename (nth 0 *df/workspaces*))
(dolist (workspace (cdr *df/workspaces*))
  (stumpwm:gnewbg workspace))


;; define keybindings

;; Apps
(define-key *top-map* (kbd "s-RET") "exec kitty --directory=/home/ben/Code/dotfiles/guix")
(define-key *top-map* (kbd "s-o") "edit-config home")
(define-key *top-map* (kbd "s-O") "edit-config system")
(define-key *top-map* (kbd "s-g") "guix-system")
(define-key *top-map* (kbd "s-G") "guix-home")
(define-key *top-map* (kbd "s-S-RET") "exec brave")
(define-key *top-map* (kbd "s-B") "exec brave")
(define-key *top-map* (kbd "s-D") "exec discord")
(define-key *top-map* (kbd "s-y") "exec kitty yazi")
(define-key *top-map* (kbd "s-F") "exec kitty yazi")
(define-key *top-map* (kbd "s-e") "emacs")
(define-key *top-map* (kbd "s-SPC") "launch-rofi")

;; Omarchy-style clipboard
(define-key *top-map* (kbd "s-c") "unified-copy")
(define-key *top-map* (kbd "s-x") "unified-cut")
(define-key *top-map* (kbd "s-v") "unified-paste")
(define-key *top-map* (kbd "s-C-v") "clipboard-history")

(define-key *top-map* (kbd "s-a") "audio-switch")
(define-key *top-map* (kbd "s-b") "cycle-wallpaper")
(define-key *top-map* (kbd "s-d") "rofi-window")
(define-key *top-map* (kbd "s-L") "exec /home/ben/Code/dotfiles/guix/scripts/lock")
(define-key *top-map* (kbd "s-p") "screenshot-screen")
(define-key *top-map* (kbd "s-P") "screenshot-region")
(define-key *top-map* (kbd "Print") "screenshot-region")
(define-key *top-map* (kbd "S-Print") "screenshot-screen")
(define-key *top-map* (kbd "M-Print") "screenrecord-region")
(define-key *top-map* (kbd "C-M-Print") "screenrecord-screen")

;; Menu (Omarchy: Super+Alt+Space main menu, Super+Escape system) and keybinding help
(define-key *top-map* (kbd "M-s-SPC") "menu")
(define-key *top-map* (kbd "s-;") "menu")
(define-key *top-map* (kbd "s-Escape") "menu system")
(define-key *top-map* (kbd "s-:") "colon")
(define-key *top-map* (kbd "s-K") "show-keybindings")

;; Focus, move and swap (hjkl: j=left, k=right, h=down, l=up)
(define-key *top-map* (kbd "s-j") "move-focus left")
(define-key *top-map* (kbd "s-k") "move-focus right")
(define-key *top-map* (kbd "s-h") "move-focus down")
(define-key *top-map* (kbd "s-l") "move-focus up")
(define-key *top-map* (kbd "s-Left") "move-focus left")
(define-key *top-map* (kbd "s-Right") "move-focus right")
(define-key *top-map* (kbd "s-Up") "move-focus up")
(define-key *top-map* (kbd "s-Down") "move-focus down")

(define-key *top-map* (kbd "s-C-j") "move-window left")
(define-key *top-map* (kbd "s-C-k") "move-window right")
(define-key *top-map* (kbd "s-C-h") "move-window down")
(define-key *top-map* (kbd "s-C-l") "move-window up")

(define-key *top-map* (kbd "s-S-Left") "exchange-direction left")
(define-key *top-map* (kbd "s-S-Right") "exchange-direction right")
(define-key *top-map* (kbd "s-S-Up") "exchange-direction up")
(define-key *top-map* (kbd "s-S-Down") "exchange-direction down")

;; Resize (Omarchy: SUPER -/= width, SUPER SHIFT -/= height)
(define-key *top-map* (kbd "s-minus") "resize -50 0")
(define-key *top-map* (kbd "s-equal") "resize 50 0")
(define-key *top-map* (kbd "s-underscore") "resize 0 -50")
(define-key *top-map* (kbd "s-plus") "resize 0 50")
(define-key *top-map* (kbd "C-s-r") "iresize")

;; Windows and frames
(define-key *top-map* (kbd "s-Q") "quit")
(define-key *top-map* (kbd "s-R") "restart-hard")
(define-key *top-map* (kbd "s-w") "delete")
(define-key *top-map* (kbd "s-q") "delete")
(define-key *top-map* (kbd "s-r") "remove")
(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-V") "toggle-float")
(define-key *top-map* (kbd "s-s") "hsplit-and-focus")
(define-key *top-map* (kbd "s-S") "vsplit-and-focus")
(define-key *top-map* (kbd "M-Tab") "pull-hidden-next")
(define-key *top-map* (kbd "M-ISO_Left_Tab") "pull-hidden-previous")

;; Workspaces
(define-key *top-map* (kbd "s-Tab") "gnext")
(define-key *top-map* (kbd "s-ISO_Left_Tab") "gprev")
(define-key *top-map* (kbd "C-s-Tab") "gother")

(define-key *top-map* (kbd "s-1") "gselect dev")
(define-key *top-map* (kbd "s-2") "gselect web")
(define-key *top-map* (kbd "s-3") "gselect term")
(define-key *top-map* (kbd "s-4") "gselect mail")
(define-key *top-map* (kbd "s-5") "gselect sys")

;; Super+Shift+digit produces the shifted keysym on a US layout
(define-key *top-map* (kbd "s-exclam") "gmove-and-follow dev")
(define-key *top-map* (kbd "s-at") "gmove-and-follow web")
(define-key *top-map* (kbd "s-numbersign") "gmove-and-follow term")
(define-key *top-map* (kbd "s-dollar") "gmove-and-follow mail")
(define-key *top-map* (kbd "s-percent") "gmove-and-follow sys")

(define-key *top-map* (kbd "C-s-1") "gmove dev")
(define-key *top-map* (kbd "C-s-2") "gmove web")
(define-key *top-map* (kbd "C-s-3") "gmove term")
(define-key *top-map* (kbd "C-s-4") "gmove mail")
(define-key *top-map* (kbd "C-s-5") "gmove sys")

;; Notifications
(define-key *top-map* (kbd "s-comma") "exec dunstctl close")
(define-key *top-map* (kbd "s-less") "exec dunstctl close-all")
(define-key *top-map* (kbd "C-s-comma") "notifications-toggle-silence")
(define-key *top-map* (kbd "M-s-comma") "exec dunstctl action")
(define-key *top-map* (kbd "M-s-less") "exec dunstctl history-pop")

;; Look and controls
(define-key *top-map* (kbd "s-S-BackSpace") "toggle-gaps")
(define-key *top-map* (kbd "s-S-SPC") "exec polybar-msg cmd toggle")
(define-key *top-map* (kbd "C-s-SPC") "cycle-wallpaper")
(define-key *top-map* (kbd "C-s-S-SPC") "exec /home/ben/Code/dotfiles/guix/scripts/theme-switch")
(define-key *top-map* (kbd "C-s-a") "exec pavucontrol")
(define-key *top-map* (kbd "C-s-b") "exec blueman-manager")
(define-key *top-map* (kbd "C-s-w") "exec kitty nmtui")
(define-key *top-map* (kbd "C-s-t") "exec kitty btop")

;; Hardware keys
(define-key *top-map* (kbd "XF86MonBrightnessUp") "bright-up")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "bright-down")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "vol-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "vol-down")
(define-key *top-map* (kbd "M-XF86AudioRaiseVolume") "volume-adjust +1%")
(define-key *top-map* (kbd "M-XF86AudioLowerVolume") "volume-adjust -1%")
(define-key *top-map* (kbd "XF86AudioMute") "vol-toggle")
(define-key *top-map* (kbd "s-XF86AudioMute") "audio-switch")
(define-key *top-map* (kbd "XF86AudioMicMute") "mic-toggle")
(define-key *top-map* (kbd "XF86AudioPlay") "exec playerctl play-pause")
(define-key *top-map* (kbd "XF86AudioPause") "exec playerctl play-pause")
(define-key *top-map* (kbd "XF86AudioNext") "exec playerctl next")
(define-key *top-map* (kbd "XF86AudioPrev") "exec playerctl previous")

(set-msg-border-width 2)

;; window placement rules
(define-frame-preference "dev"
    (1 t t :class "Emacs"))

(define-frame-preference "web"
    (2 t t :class "Brave-browser")
  (2 t t :class "discord"))

;; start processes
(defun spawn-once (pgrep-args command)
  "Run COMMAND unless `pgrep PGREP-ARGS' finds it already running.
restart-hard reloads this file, which would otherwise stack duplicates. The
check runs in its own shell: in `pgrep -f X || exec X' that shell's command
line contains X, so pgrep matched the shell itself and nothing ever started."
  (when (string= "" (run-shell-command (format nil "pgrep ~a" pgrep-args) t))
    (run-shell-command (format nil "exec ~a" command))))

(run-commands
 "start-polybar"
 "gselect dev")
(run-shell-command "setxkbmap -option '' -option caps:ctrl_modifier us && xset r rate 100 100 && { pgrep -x xcape >/dev/null || exec xcape -e 'Caps_Lock=Escape'; }")
(run-shell-command "/home/ben/Code/dotfiles/guix/scripts/wallpaper restore")
(spawn-once "-x picom" "picom --config /home/ben/Code/dotfiles/picom/picom.conf")
;; Guix wraps these, so match the command line; [x] keeps pgrep from matching this shell
(spawn-once "-f '[c]lipmenud'" "clipmenud")
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "mkdir -p ~/Screenshots")
;; theme-switch generates this from dunst/dunstrc with the theme's colors
(spawn-once "-x dunst" "dunst -config /home/ben/.config/theme/current/dunstrc")
(spawn-once "-f '[n]m-applet'" "nm-applet")
;; Lock after 5 idle minutes and before suspend; guix/scripts/idle sets the
;; X screensaver and DPMS timeouts that xss-lock reacts to
(run-shell-command "/home/ben/Code/dotfiles/guix/scripts/idle on")
(spawn-once "-f '[x]ss-lock'" "xss-lock -- /home/ben/Code/dotfiles/guix/scripts/lock --wait")

;; gaps
(asdf:load-system :swm-gaps)
(setf swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)
(swm-gaps:toggle-gaps-on)

;; Polybar
(defun polybar-groups ()
  "Workspace numbers for the polybar workspaces module: the current one in the
accent color and underlined, occupied ones in the foreground color, empty ones
muted. A click switches workspace with xdotool (StumpWM handles
_NET_CURRENT_DESKTOP); stumpish can't reach StumpWM from polybar."
  (let ((groups (sort (copy-list (screen-groups (current-screen))) #'< :key #'group-number)))
    (format nil "~{~a~}"
            (loop for group in groups
                  for index from 0
                  collect (format nil "%{A1:xdotool set_desktop ~d:}~a ~d %{F- -u}%{A}"
                                  index
                                  (cond ((eq group (current-group))
                                         (format nil "%{F~a u~a +u}" *theme-blue* *theme-blue*))
                                        ((group-windows group)
                                         (format nil "%{F~a}" *theme-fg*))
                                        (t (format nil "%{F~a}" *theme-muted*)))
                                  (group-number group))))))

(defun polybar-update-groups ()
  (run-shell-command (concat "polybar-msg action workspaces send '"
                             (polybar-groups) "'")))

;; Named hook functions: reloading this file replaces them instead of adding copies
(defun polybar-on-window (win)
  (declare (ignore win))
  (polybar-update-groups))

(defun polybar-on-focus (new old)
  (declare (ignore new old))
  (polybar-update-groups))

(remove-hook *new-window-hook* 'polybar-on-window)
(add-hook *new-window-hook* 'polybar-on-window)
(remove-hook *destroy-window-hook* 'polybar-on-window)
(add-hook *destroy-window-hook* 'polybar-on-window)
(remove-hook *focus-window-hook* 'polybar-on-focus)
(add-hook *focus-window-hook* 'polybar-on-focus)
(remove-hook *focus-group-hook* 'polybar-on-focus)
(add-hook *focus-group-hook* 'polybar-on-focus)

;; TTF fonts
(asdf:load-system :clx-truetype)
(asdf:load-system :ttf-fonts)
(setf xft:*font-dirs* '("/home/ben/.guix-home/profile/share/fonts/truetype/"
                        "/home/ben/.guix-home/profile/share/fonts/opentype/"))
;; clx-truetype fixes its cache path at build time, which points into the
;; read-only store; cache-fonts would fail and abort the rest of this file.
(setf xft:+font-cache-filename+
      (merge-pathnames ".cache/stumpwm/font-cache.sexp" (user-homedir-pathname)))
(xft:cache-fonts)
(defun apply-theme-font ()
  "Use *theme-font* for StumpWM messages. xft only sees fonts in the Guix home
profile, so a family it can't load keeps the current font."
  (handler-case
      (set-font (make-instance 'xft:font :family *theme-font* :subfamily "Regular" :size 14))
    (error ()
      (dformat 1 "StumpWM can't load font ~a~%" *theme-font*))))

(apply-theme-font)

;; Slynk REPL (uncomment to connect Sly/Slime to StumpWM)
;; (require :slynk)
;; (slynk:create-server :port 4009 :dont-close t)
