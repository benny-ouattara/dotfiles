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

(defcommand start-firefox () ()
  "Run or raise firefox web browser."
  (run-or-raise "firefox" '(:class "Firefox") t nil))

(defcommand start-emacs () ()
  "Run or raise emacs."
  (run-or-raise "emacs" '(:class "Emacs") t nil))

(defcommand start-kitty () ()
  "Run or raise kitty."
  (run-or-raise "kitty" '(:class "kitty") t nil))

(defcommand unified-copy () ()
  "Copy: send M-w to Emacs, C-c to everything else."
  (let ((win (current-window)))
    (if (string-equal (window-class win) "Emacs")
        (send-fake-key win (kbd "M-w"))
        (run-shell-command "xdotool key ctrl+c"))))

(defcommand unified-cut () ()
  "Cut: send C-w to Emacs, C-x to everything else."
  (let ((win (current-window)))
    (if (string-equal (window-class win) "Emacs")
        (send-fake-key win (kbd "C-w"))
        (run-shell-command "xdotool key ctrl+x"))))

(defcommand unified-paste () ()
  "Paste: send C-y to Emacs, C-v to everything else."
  (let ((win (current-window)))
    (if (string-equal (window-class win) "Emacs")
        (send-fake-key win (kbd "C-y"))
        (run-shell-command "xdotool key ctrl+v"))))

(defcommand clipboard-history () ()
  "Show clipboard history via clipmenu with rofi."
  (run-shell-command "clipmenu"))

(defcommand show-keybindings () ()
  "Display keybindings via rofi."
  (run-shell-command
   (concatenate 'string
                "echo -e '"
                "s-RET        Terminal\\n"
                "s-S-RET      Browser\\n"
                "s-SPC        App launcher\\n"
                "s-e          Emacs\\n"
                "s-o / s-O    Edit system / home config\\n"
                "s-c          Copy\\n"
                "s-x          Cut\\n"
                "s-v          Paste\\n"
                "s-C-v        Clipboard history\\n"
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
  (run-shell-command "polybar-msg cmd quit 2>/dev/null; sleep 0.5; polybar main &"))

(defun rofi (mode)
  (run-shell-command (concat "rofi -show " mode " -m " (write-to-string (head-number (current-head))))))

(defcommand rofi-run () ()
  (rofi "run -sidebar-mode"))

(defcommand launch-rofi () ()
  (rofi "drun -theme /home/ben/.config/rofi/launchers/type-1/style-5.rasi"))

(defcommand rofi-window () ()
  (rofi "window"))

(defcommand rofi-windowcd () ()
  (rofi "windowcd"))

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
(define-key *top-map* (kbd "s-o") "exec kitty --directory=/home/ben/Code/dotfiles/guix emacsclient -t otter-system.scm")
(define-key *top-map* (kbd "s-O") "exec kitty --directory=/home/ben/Code/dotfiles/guix emacsclient -t otter-home.scm")
(define-key *top-map* (kbd "s-g") "guix-system")
(define-key *top-map* (kbd "s-G") "guix-home")
(define-key *top-map* (kbd "s-w") "exec firefox")
(define-key *top-map* (kbd "s-S-RET") "exec firefox")
(define-key *top-map* (kbd "s-e") "emacs")

;; Omarchy-style clipboard
(define-key *top-map* (kbd "s-c") "unified-copy")
(define-key *top-map* (kbd "s-x") "unified-cut")
(define-key *top-map* (kbd "s-v") "unified-paste")
(define-key *top-map* (kbd "s-C-v") "clipboard-history")

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

(define-key *top-map* (kbd "XF86MonBrightnessUp") "backlight-up")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "backlight-down")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "volume-toggle-mute")

(set-msg-border-width 2)

;; window placement rules
(define-frame-preference "dev"
    (1 t t :class "Emacs"))

(define-frame-preference "web"
    (2 t t :class "Firefox"))

;; start processes
(run-commands
 "start-polybar"
 "start-firefox"
 "start-emacs"
 "gselect dev")
(run-shell-command "setxkbmap us -option 'caps:ctrl_modifier'")
(run-shell-command "xcape -e 'Caps_Lock=Escape'")
(run-shell-command "xset r rate 100 100")
(run-shell-command "feh --randomize --bg-fill ~/Sync/wallpapers/*")
(run-shell-command "picom")
(run-shell-command "clipmenud")
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "amixer")

;; gaps
(asdf:load-system :swm-gaps)
(setf swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)
(swm-gaps:toggle-gaps-on)

(asdf:load-system :screenshot)

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
  (run-shell-command "polybar-msg action '#stumpwmgroups.hook.0'"))

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

(run-shell-command "nm-applet")

;; Slynk REPL (uncomment to connect Sly/Slime to StumpWM)
;; (require :slynk)
;; (slynk:create-server :port 4009 :dont-close t)
