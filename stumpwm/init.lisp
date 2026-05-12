#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; Unbind prefix key - all bindings are on Super directly
(define-key *top-map* (kbd "C-a") nil)
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
(redirect-all-output (data-dir-file "debug" "log"))

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

(defcommand unified-copy () ()
  "Copy: in Emacs send M-w, otherwise promote X PRIMARY selection to CLIPBOARD."
  (let ((win (current-window)))
    (if (string-equal (window-class win) "Emacs")
        (send-fake-key win (kbd "M-w"))
        (run-shell-command "xclip -selection primary -o | xclip -selection clipboard -i"))))

(defcommand unified-cut () ()
  "Cut: send C-w to Emacs, copy PRIMARY to CLIPBOARD for everything else."
  (let ((win (current-window)))
    (if (string-equal (window-class win) "Emacs")
        (send-fake-key win (kbd "C-w"))
        (run-shell-command "xclip -selection primary -o | xclip -selection clipboard -i"))))

(defcommand unified-paste () ()
  "Paste: send C-y to Emacs, type CLIPBOARD contents for everything else."
  (let ((win (current-window)))
    (if (string-equal (window-class win) "Emacs")
        (send-fake-key win (kbd "C-y"))
        (run-shell-command "xdotool type --clearmodifiers -- \"$(xclip -selection clipboard -o | tr -d '\\n')\""))))

(defcommand clipboard-history () ()
  "Show clipboard history via clipmenu with rofi."
  (run-shell-command "CM_LAUNCHER=rofi clipmenu -theme ~/.config/rofi/launchers/type-1/style-8.rasi"))

(defcommand cycle-wallpaper () ()
  "Set a random wallpaper."
  (run-shell-command "feh --randomize --bg-fill ~/Sync/wallpapers/*"))

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

(defcommand vol-up () ()
  "Raise volume and show notification."
  (run-shell-command "pactl set-sink-volume @DEFAULT_SINK@ +5% && notify-send -h string:x-dunst-stack-tag:volume \"Volume\" \"$(pactl get-sink-volume @DEFAULT_SINK@ | grep -oP '\\d+%' | head -1)\""))

(defcommand vol-down () ()
  "Lower volume and show notification."
  (run-shell-command "pactl set-sink-volume @DEFAULT_SINK@ -5% && notify-send -h string:x-dunst-stack-tag:volume \"Volume\" \"$(pactl get-sink-volume @DEFAULT_SINK@ | grep -oP '\\d+%' | head -1)\""))

(defcommand vol-toggle () ()
  "Toggle mute and show notification."
  (run-shell-command "pactl set-sink-mute @DEFAULT_SINK@ toggle && notify-send -h string:x-dunst-stack-tag:volume \"Volume\" \"$(pactl get-sink-mute @DEFAULT_SINK@ | cut -d: -f2)\""))

(defcommand bright-up () ()
  "Raise brightness and show notification."
  (run-shell-command "brightnessctl set +10% && notify-send -h string:x-dunst-stack-tag:brightness \"Brightness\" \"$(brightnessctl -m | cut -d, -f4)\""))

(defcommand bright-down () ()
  "Lower brightness and show notification."
  (run-shell-command "brightnessctl set 10%- && notify-send -h string:x-dunst-stack-tag:brightness \"Brightness\" \"$(brightnessctl -m | cut -d, -f4)\""))

(defcommand screenshot-screen () ()
  "Take a fullscreen screenshot."
  (run-shell-command "maim ~/Screenshots/$(date +%Y%m%d-%H%M%S).png && notify-send 'Screenshot saved'"))

(defcommand screenshot-region () ()
  "Take a screenshot of a selected region."
  (run-shell-command "maim -s ~/Screenshots/$(date +%Y%m%d-%H%M%S).png && notify-send 'Screenshot saved'"))

(defcommand show-keybindings () ()
  "Display keybindings via rofi."
  (run-shell-command
   (concatenate 'string
                "echo -e '"
                "s-RET        Terminal\\n"
                "s-S-RET      Brave browser\\n"
                "s-D          Discord\\n"
                "s-SPC        App launcher\\n"
                "s-e          Emacs\\n"
                "s-o / s-O    Edit system / home config\\n"
                "s-c          Copy\\n"
                "s-x          Cut\\n"
                "s-v          Paste\\n"
                "s-C-v        Clipboard history\\n"
                "s-a          Switch audio output\\n"
                "s-b          Random wallpaper\\n"
                "s-Tab        Cycle windows\\n"
                "s-d          Window list\\n"
                "C-s-l        Lock screen\\n"
                "s-p / s-P    Screenshot / region\\n"
                "s-j/k/h/l    Focus direction\\n"
                "s-C-j/k/h/l  Move window\\n"
                "M-j/k/h/l    Resize direction\\n"
                "s-s / s-S    HSplit / VSplit\\n"
                "s-f          Fullscreen\\n"
                "s-q          Close window\\n"
                "s-r          Remove frame\\n"
                "s-1..5       Switch workspace\\n"
                "C-s-1..5     Move to workspace\\n"
                "s-g / s-G    Guix system / home\\n"
                "s-;          System menu\\n"
                "s-:          Command prompt\\n"
                "s-R          Restart StumpWM\\n"
                "s-Q          Quit StumpWM\\n"
                "s-K          This help"
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
(define-key *top-map* (kbd "M-k") "resize-direction Right")
(define-key *top-map* (kbd "M-j") "resize-direction Left")
(define-key *top-map* (kbd "M-l") "resize-direction Up")
(define-key *top-map* (kbd "M-h") "resize-direction Down")

(define-key *top-map* (kbd "s-RET") "exec kitty --directory=/home/ben/Code/dotfiles/guix")
(define-key *top-map* (kbd "s-o") "exec kitty --directory=/home/ben/Code/dotfiles/guix emacsclient -t home/config.scm")
(define-key *top-map* (kbd "s-O") "exec kitty --directory=/home/ben/Code/dotfiles/guix emacsclient -t system/config.scm")
(define-key *top-map* (kbd "s-g") "guix-system")
(define-key *top-map* (kbd "s-G") "guix-home")
(define-key *top-map* (kbd "s-w") "exec brave")
(define-key *top-map* (kbd "s-S-RET") "exec brave")
(define-key *top-map* (kbd "s-D") "exec discord")
(define-key *top-map* (kbd "s-y") "exec kitty yazi")
(define-key *top-map* (kbd "s-e") "emacs")

;; Omarchy-style clipboard
(define-key *top-map* (kbd "s-c") "unified-copy")
(define-key *top-map* (kbd "s-x") "unified-cut")
(define-key *top-map* (kbd "s-v") "unified-paste")
(define-key *top-map* (kbd "s-C-v") "clipboard-history")

(define-key *top-map* (kbd "s-a") "audio-switch")
(define-key *top-map* (kbd "s-b") "cycle-wallpaper")
(define-key *top-map* (kbd "s-Tab") "pull-hidden-next")
(define-key *top-map* (kbd "s-d") "rofi-window")
(define-key *top-map* (kbd "C-s-l") "exec slock")
(define-key *top-map* (kbd "s-p") "screenshot-screen")
(define-key *top-map* (kbd "s-P") "screenshot-region")

;; System menu and keybinding help
(define-key *top-map* (kbd "s-;") "system-menu")
(define-key *top-map* (kbd "s-:") "colon")
(define-key *top-map* (kbd "s-K") "show-keybindings")

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
(define-key *top-map* (kbd "s-SPC") "launch-rofi")
(define-key *top-map* (kbd "C-s-r") "iresize")

(define-key *top-map* (kbd "s-1") "gselect dev")
(define-key *top-map* (kbd "s-2") "gselect web")
(define-key *top-map* (kbd "s-3") "gselect term")
(define-key *top-map* (kbd "s-4") "gselect mail")
(define-key *top-map* (kbd "s-5") "gselect sys")

(define-key *top-map* (kbd "C-s-1") "gmove dev")
(define-key *top-map* (kbd "C-s-2") "gmove web")
(define-key *top-map* (kbd "C-s-3") "gmove term")
(define-key *top-map* (kbd "C-s-4") "gmove mail")
(define-key *top-map* (kbd "C-s-5") "gmove sys")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "bright-up")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "bright-down")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "vol-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "vol-down")
(define-key *top-map* (kbd "XF86AudioMute") "vol-toggle")

(set-msg-border-width 2)

;; window placement rules
(define-frame-preference "dev"
    (1 t t :class "Emacs"))

(define-frame-preference "web"
    (2 t t :class "Brave-browser")
    (2 t t :class "discord"))

;; start processes
(run-commands
 "start-polybar"
 "gselect dev")
(run-shell-command "setxkbmap us -option 'caps:ctrl_modifier'")
(run-shell-command "xcape -e 'Caps_Lock=Escape'")
(run-shell-command "xset r rate 100 100")
(run-shell-command "feh --randomize --bg-fill ~/Sync/wallpapers/*")
(run-shell-command "picom --config /home/ben/Code/dotfiles/picom/picom.conf")
(run-shell-command "clipmenud")
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "mkdir -p ~/Screenshots")
(run-shell-command "dunst -config /home/ben/Code/dotfiles/dunst/dunstrc")

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

(add-hook *new-window-hook* (lambda (win) (polybar-update-groups)))
(add-hook *destroy-window-hook* (lambda (win) (polybar-update-groups)))
(add-hook *focus-window-hook* (lambda (win lastw) (polybar-update-groups)))
(add-hook *focus-group-hook* (lambda (grp lastg) (polybar-update-groups)))

;; TTF fonts
(asdf:initialize-source-registry
 '(:source-registry
   (:include "/home/ben/.guix-home/profile/etc/common-lisp/source-registry.conf.d/")
   :inherit-configuration))
(asdf:load-system :clx-truetype)
(asdf:load-system :ttf-fonts)
(setf xft:*font-dirs* '("/home/ben/.guix-home/profile/share/fonts/truetype/"
                        "/home/ben/.guix-home/profile/share/fonts/opentype/"))
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "Iosevka Term" :subfamily "Regular" :size 14))

;; Set DBUS_SESSION_BUS_ADDRESS for nix apps (jeepney can't handle autolaunch:)
(let ((addr (string-trim '(#\Newline #\Space)
             (run-shell-command
              "ss -xlp 2>/dev/null | grep dbus-daemon | grep -oP '/tmp/dbus-\\S+' | head -1 | xargs -I{} echo 'unix:path={}'" t))))
  (when (> (length addr) (length "unix:path="))
    (sb-posix:setenv "DBUS_SESSION_BUS_ADDRESS" addr 1)))

(run-shell-command "nm-applet")

;; Slynk REPL (uncomment to connect Sly/Slime to StumpWM)
;; (require :slynk)
;; (slynk:create-server :port 4009 :dont-close t)
