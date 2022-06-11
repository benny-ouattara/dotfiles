#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)

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

(stumpwm:add-to-load-path "~/.guix-profile/share/common-lisp/sbcl/stumpwm-swm-gaps")
(stumpwm:add-to-load-path "~/.guix-profile/share/common-lisp/sbcl/stumpwm-ttf-fonts")
(stumpwm:add-to-load-path "~/.guix-profile/share/common-lisp/sbcl/stumpwm-stumptray")
(stumpwm:add-to-load-path "~/.guix-profile/share/common-lisp/sbcl/stumpwm-kbd-layouts")

(setf *startup-message* nil)

(set-prefix-key (kbd "C-a"))
(setf *mouse-focus-policy* :click
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

;; (run-shell-command "xmodmap -e 'clear mod4'" t)
;; (run-shell-command "xmodmap -e \'keycode 133 = F20\'" t)
;; (set-prefix-key (kbd "F20"))

(defvar *df/workspaces* (list "dev" "web" "term" "random" "misc"))
(stumpwm:grename (nth 0 *df/workspaces*))
(dolist (workspace (cdr *df/workspaces*))
  (stumpwm:gnewbg workspace))

(defvar *move-to-keybinds* (list "!" "@" "#" "$" "%" "^" "&" "*" "("))
(dotimes (y (length *df/workspaces*))
  (let ((workspace (write-to-string (+ y 1))))
    (define-key *root-map* (kbd workspace) (concat "gselect " workspace))
    (define-key *root-map* (kbd (nth y *move-to-keybinds*)) (concat "gmove-and-follow " workspace))))

(setf *resize-increment* 50)
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

(set-border-color "#c792ea")
(set-bg-color "#232635")
(set-fg-color "#A6Accd")
(set-msg-border-width 2)

;; start essential processes
;; (run-shell-command "nm-applet")
(run-shell-command "syncthing")
(run-commands "start-emacs")
(run-shell-command "setxkbmap us -option 'caps:ctrl_modifier'")
(run-shell-command "xcape -e 'Caps_Lock=Escape'")
(run-shell-command "xset r rate 150 60")
(run-shell-command "feh --randomize --bg-fill ~/Sync/wallpapers/*")
(run-shell-command "picom")
(run-shell-command "xsetroot -cursor_name left_ptr")
;; (run-shell-command "volumeicon")

;; load modules last so that they don't break system in failure case
(ql:quickload :clx-truetype)
(load-module "ttf-fonts")
(setf xft:*font-dirs* '("/home/ben/.guix-profile/share/fonts/"))
;; (setf clx-truetype:+font-cache-filename+ "/home/ben/.local/share/fonts/font-cache.sexp")
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "JetBrains Mono" :subfamily "Regular" :size 16))

;; gaps
(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 7)
(run-commands "toggle-gaps-on")

;; tray
(ql:quickload :xembed)
(load-module "stumptray")
(stumptray:stumptray)

;; mode-line
;; (load-module "cpu")
;; (load-module "mem")
;; (load-module "screenshot")
;; (load-module "battery-portable")

;; (load-module "net")
;; (load-module "wifi")
;; (setf *mode-line-timeout* 2)
;; (setf *time-modeline-string* "%F %H:%M")
;; (setf *group-format* "%t")
;; (setf *window-format* "%n: %30t")
;; (setf *mode-line-background-color* "#232635")
;; (setf *mode-line-foreground-color* "#A6Accd")
;; (setf wifi:*wifi-modeline-fmt*       "%e %P"
;;       wifi:*use-colors*              nil)
;; (run-commands "mode-line")
;; (setf stumpwm:*screen-mode-line-format*
;;       (list
;;        "%n   "
;;        "^>" '(:eval (stumpwm:run-shell-command
;;                      "LANG=en_US.utf8 date +%A' '%d.%m.%Y' '%l:%M' '%p' 'GMT''%:::z'           '" t))))

;; (stumpwm:enable-mode-line (stumpwm:current-screen)
;;                           (stumpwm:current-head)
;;                           t)
;; have this be the last line of config since creating server is not an idempotent operation
(require :slynk)
(slynk:create-server :dont-close t)
