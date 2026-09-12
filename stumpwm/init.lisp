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

;; Message bar - Catppuccin Mocha
(set-fg-color "#CDD6F4")
(set-bg-color "#1E1E2E")
(set-border-color "#89B4FA")
(setf *colors*
      '("#1E1E2E"   ; 0 black  (Base)
        "#F38BA8"   ; 1 red
        "#A6E3A1"   ; 2 green
        "#F9E2AF"   ; 3 yellow
        "#89B4FA"   ; 4 blue
        "#CBA6F7"   ; 5 magenta (Mauve)
        "#94E2D5"   ; 6 cyan   (Teal)
        "#CDD6F4")) ; 7 white  (Text)
(update-color-map (current-screen))

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
  (run-shell-command "CM_LAUNCHER=rofi clipmenu -theme ~/.config/rofi/launchers/type-1/style-8.rasi"))

(defcommand cycle-wallpaper () ()
  "Set a random wallpaper."
  (run-shell-command "feh --randomize --bg-fill ~/Sync/wallpapers/*"))

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
                "rofi -dmenu -p 'Audio' -theme ~/.config/rofi/launchers/type-1/style-8.rasi | "
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
  (message "Notifications ~a"
           (if (search "true" (run-shell-command "dunstctl is-paused" t))
               "silenced"
               "on")))

(defcommand screenshot-screen () ()
  "Take a fullscreen screenshot, save it and copy it to the clipboard."
  (run-shell-command "f=~/Screenshots/$(date +%Y%m%d-%H%M%S).png; maim \"$f\" && xclip -selection clipboard -t image/png -i \"$f\" && notify-send 'Screenshot saved' \"$f\""))

(defcommand screenshot-region () ()
  "Screenshot a selected region, save it and copy it to the clipboard."
  (run-shell-command "f=~/Screenshots/$(date +%Y%m%d-%H%M%S).png; maim -s \"$f\" && xclip -selection clipboard -t image/png -i \"$f\" && notify-send 'Screenshot saved' \"$f\""))

(defcommand show-keybindings () ()
  "Display keybindings via rofi."
  (run-shell-command
   (concatenate 'string
                "echo -e '"
                "s-RET            Terminal\\n"
                "s-S-RET / s-B    Brave browser\\n"
                "s-D              Discord\\n"
                "s-e              Emacs\\n"
                "s-y / s-F        Yazi file manager\\n"
                "s-SPC            App launcher\\n"
                "s-o / s-O        Edit home / system config\\n"
                "s-c / s-v        Copy / paste\\n"
                "s-x              Cut (copy outside Emacs)\\n"
                "s-C-v            Clipboard history\\n"
                "s-w / s-q        Close window\\n"
                "s-j/k/h/l        Focus left/right/down/up\\n"
                "s-Arrows         Focus direction\\n"
                "s-C-j/k/h/l      Move window left/right/down/up\\n"
                "s-S-Arrows       Swap window\\n"
                "s-- / s-=        Narrower / wider\\n"
                "s-_ / s-+        Shorter / taller\\n"
                "C-s-r            Interactive resize\\n"
                "s-V              Float / tile window\\n"
                "s-f              Fullscreen\\n"
                "s-s / s-S        HSplit / VSplit\\n"
                "s-r              Remove frame\\n"
                "s-d              Window list\\n"
                "M-Tab / M-S-Tab  Next / previous window\\n"
                "s-1..5           Switch workspace\\n"
                "s-S-1..5         Move window and follow\\n"
                "C-s-1..5         Move window\\n"
                "s-Tab / s-S-Tab  Next / previous workspace\\n"
                "C-s-Tab          Last workspace\\n"
                "s-Esc / s-;      System menu\\n"
                "s-:              Command prompt\\n"
                "s-g / s-G        Guix system / home\\n"
                "C-s-l            Lock screen\\n"
                "s-p / S-Print    Screenshot screen\\n"
                "s-P / Print      Screenshot region\\n"
                "s-a / s-Mute     Switch audio output\\n"
                "M-Vol keys       Volume by 1%\\n"
                "C-s-a            Audio mixer\\n"
                "C-s-b            Bluetooth\\n"
                "C-s-w            Network\\n"
                "C-s-t            Activity (btop)\\n"
                "s-, / s-S-,      Dismiss / dismiss all notifications\\n"
                "C-s-,            Silence notifications\\n"
                "M-s-,            Notification action\\n"
                "M-s-S-,          Restore last notification\\n"
                "s-b / C-s-SPC    Random wallpaper\\n"
                "C-s-S-SPC        Switch theme\\n"
                "s-S-BackSpace    Toggle gaps\\n"
                "s-S-SPC          Toggle bar\\n"
                "s-R / s-Q        Restart / quit StumpWM\\n"
                "s-K              This help"
                "' | rofi -dmenu -i -p 'Keys' -theme ~/.config/rofi/launchers/type-1/style-8.rasi")))

(defcommand system-menu () ()
  "Show system menu via rofi."
  (run-shell-command
   (concatenate 'string
                "choice=$(echo -e "
                "'Guix System Reconfigure\\n"
                "Guix Home Reconfigure\\n"
                "Guix Pull\\n"
                "Guix Garbage Collect\\n"
                "Guix Rollback\\n"
                "Guix Status\\n"
                "Guix Health\\n"
                "Switch Theme\\n"
                "Restart StumpWM\\n"
                "Quit StumpWM\\n"
                "Lock Screen\\n"
                "Edit System Config\\n"
                "Edit Home Config' "
                "| rofi -dmenu -p 'System' -theme ~/.config/rofi/launchers/type-1/style-8.rasi); "
                "case \"$choice\" in "
                "'Guix System Reconfigure') "
                "exec kitty zsh -c 'up system-reconfigure; exec zsh;' ;; "
                "'Guix Home Reconfigure') "
                "exec kitty zsh -c 'up home-reconfigure; exec zsh;' ;; "
                "'Guix Pull') "
                "exec kitty zsh -c 'up pull; exec zsh;' ;; "
                "'Guix Garbage Collect') "
                "exec kitty zsh -c 'up gc-safe; exec zsh;' ;; "
                "'Guix Rollback') "
                "exec kitty zsh -c 'up rollback; exec zsh;' ;; "
                "'Guix Status') "
                "exec kitty zsh -c 'up status; exec zsh;' ;; "
                "'Guix Health') "
                "exec kitty zsh -c 'up health; exec zsh;' ;; "
                "'Switch Theme') /home/ben/Code/dotfiles/guix/scripts/theme-switch ;; "
                "'Restart StumpWM') stumpish restart-hard ;; "
                "'Quit StumpWM') stumpish quit ;; "
                "'Lock Screen') slock ;; "
                "'Edit System Config') emacsclient -c /home/ben/Code/dotfiles/guix/system/config.scm ;; "
                "'Edit Home Config') emacsclient -c /home/ben/Code/dotfiles/guix/home/config.scm ;; "
                "esac")))

(defcommand start-slynk (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (slynk:create-server :port (parse-integer port) :dont-close t))
   :name "manual-slynk-stumpwm"))

(defcommand start-polybar () ()
  "Kill existing polybar and start fresh."
  (run-shell-command "polybar-msg cmd quit 2>/dev/null; sleep 0.5; polybar --config=/home/ben/Code/dotfiles/polybar/tokyo/config.ini main &"))

(defun rofi (mode)
  (run-shell-command (concat "rofi -show " mode " -m " (write-to-string (head-number (current-head))) " -theme ~/.config/rofi/launchers/type-1/style-8.rasi")))

(defcommand launch-rofi () ()
  (rofi "drun"))

(defcommand rofi-window () ()
  (rofi "window"))

(defun guix-run (cmd)
  (gselect "sys")
  (run-shell-command cmd))

(defcommand guix-system () ()
  "Reconfigure guix system."
  (guix-run "exec kitty zsh -c 'up system-reconfigure; exec zsh;'"))

(defcommand guix-home () ()
  "Reconfigure guix home."
  (guix-run "exec kitty zsh -c 'up home-reconfigure; exec zsh;'"))

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
(define-key *top-map* (kbd "s-o") "exec kitty --directory=/home/ben/Code/dotfiles/guix emacsclient -t home/config.scm")
(define-key *top-map* (kbd "s-O") "exec kitty --directory=/home/ben/Code/dotfiles/guix emacsclient -t system/config.scm")
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
(define-key *top-map* (kbd "C-s-l") "exec slock")
(define-key *top-map* (kbd "s-p") "screenshot-screen")
(define-key *top-map* (kbd "s-P") "screenshot-region")
(define-key *top-map* (kbd "Print") "screenshot-region")
(define-key *top-map* (kbd "S-Print") "screenshot-screen")

;; System menu and keybinding help
(define-key *top-map* (kbd "s-;") "system-menu")
(define-key *top-map* (kbd "s-Escape") "system-menu")
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
restart-hard reloads this file, which would otherwise stack duplicates."
  (run-shell-command (format nil "pgrep ~a >/dev/null || exec ~a" pgrep-args command)))

(run-commands
 "start-polybar"
 "gselect dev")
(run-shell-command "setxkbmap -option '' -option caps:ctrl_modifier us && xset r rate 100 100 && { pgrep -x xcape >/dev/null || exec xcape -e 'Caps_Lock=Escape'; }")
(run-shell-command "feh --randomize --bg-fill ~/Sync/wallpapers/*")
(spawn-once "-x picom" "picom --config /home/ben/Code/dotfiles/picom/picom.conf")
;; Guix wraps these, so match the command line; [x] keeps pgrep from matching this shell
(spawn-once "-f '[c]lipmenud'" "clipmenud")
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "mkdir -p ~/Screenshots")
(spawn-once "-x dunst" "dunst -config /home/ben/Code/dotfiles/dunst/dunstrc")
(spawn-once "-f '[n]m-applet'" "nm-applet")

;; gaps
(asdf:load-system :swm-gaps)
(setf swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)
(swm-gaps:toggle-gaps-on)

;; Polybar
(defun icon-by-group (name)
  (cond
    ((string-equal name "dev")
     "")
    ((string-equal name "web")
     "")
    ((string-equal name "term")
     "")
    ((string-equal name "mail")
     "")
    ((string-equal name "sys")
     "")
    (t (concat ""))))

(defun polybar-groups ()
  "Return string representation for polybar stumpgroups module"
  (apply #'concatenate 'string
         (mapcar
          (lambda (g)
            (let* ((name (group-name g))
                   (number (write-to-string (group-number g)))
                   (n-win (write-to-string (length (group-windows g))))
                   (icon (icon-by-group name))
                   (text (concat " %{F#89B4FA}" icon "%{F-} " number ":" name " ")))
              (cond
                ((eq g (current-group)) (concat "%{F#CDD6F4 B#313244 u#89B4FA +u}" text "[" n-win "] " "%{F- B- u- -u}"))
                ((string-equal n-win "0") "")
                (t (concat "%{F#CDD6F4}" text "[" n-win "] " "%{F-}")))))
          (sort (screen-groups (current-screen)) #'< :key #'group-number))))

(defun polybar-update-groups ()
  (run-shell-command (concat "polybar-msg action stumpwmgroups send '"
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

;; Polybar starts in the background; fill the groups module once it is up
(run-with-timer 2 nil 'polybar-update-groups)

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
(set-font (make-instance 'xft:font :family "Iosevka Term" :subfamily "Regular" :size 14))

;; Slynk REPL (uncomment to connect Sly/Slime to StumpWM)
;; (require :slynk)
;; (slynk:create-server :port 4009 :dont-close t)
