;;; themes/bluloco-dark-theme.el -*- lexical-binding: t; -*-
;; implement https://github.com/uloco/theme-bluloco-dark

(require 'doom-themes)

(defgroup bluloco-dark-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(def-doom-theme bluloco-dark-theme
  "Dark theme for bluloco"

  ;; define color palette
  ;; name        default   256          16
  ((bg            '("#282c34" "#282c34" nil            ))
   (fg            '("#abb2bf" "#abb2bf" "black"        ))
   (base0         '("#ff78f8" "#ff78f8" nil            ))
   (base1         '("#9f7efe" "#9f7efe" nil            ))
   (base2         '("#3691ff" "#3691ff" nil            ))
   (base3         '("#ff936a" "#ff936a" nil            ))
   (base4         '("#ff6480" "#ff6480" nil            ))
   (base5         '("#7a82da" "#7a82da" nil            ))
   (base6         '("#636d83" "#636d83" nil            ))
   (base7         '("#10b1fe" "#10b1fe" nil            ))
   (base8         '("#3fc56b" "#3fc56b" nil            ))
   (base9         '("#ce9887" "#ce9887" nil            ))
   (base10        '("#f9c859" "#f9c859" nil            ))
   (base11        '("#8c8c84" "#9ca0a4" "brightblack"  ))
   (base12        '("#1b2229" "black"   "black"        ))

   (highlight      blue)
   (vertical-bar   base2)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base6)
   (doc-comments   (doom-lighten comments 0.15))
   (constants      base1)
   (functions      base8)
   (keywords       base7)
   (methods        base8)
   (operators      blue)
   (type           base4)
   (strings        base10)
   (variables      base1)
   (numbers        base0)
   (region         baby-blue)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   (modeline-fg     nil)
   (modeline-fg-alt (doom-blend violet base4 0.2))

   (modeline-bg base1)
   (modeline-bg-l base2)
   (modeline-bg-inactive (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))

  ((font-lock-comment-face
    :foreground comments
    :weight 'bold)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments
    :weight 'regular)

   ((line-number &override) :foreground base11)
   ((line-number-current-line &override) :foreground base12)

   (doom-modeline-bar :background highlight)
   (doom-modeline-project-dir :foreground violet :weight 'bold)
   (doom-modeline-buffer-file :weight 'regular)

   (mode-line :background modeline-bg :foreground modeline-fg)
   (mode-line-inactive :background modeline-bg-inactive :foreground modeline-fg-alt)
   (mode-line-emphasis :foreground highlight)

   (org-block            :background base0)
   ;; (org-level-1          :foreground base8 :weight 'bold :height 1.25)
   ;; (org-level-2          :foreground base6 :weight 'bold :height 1.1)
   ;; (org-level-3          :foreground base5 :bold bold :height 1.0)
   ;; (org-level-4          :foreground base4 :bold bold :height 1.0)
   ;; (org-ellipsis         :underline nil :background bg-alt     :foreground grey)
   ;; (org-quote            :background base1)
   ;; (org-checkbox-statistics-done :foreground base2 :weight 'normal)
   ;; (org-done nil)
   ;; (org-done :foreground green :weight 'normal)
   ;; (org-headline-done :foreground base3 :weight 'normal :strike-through t)
   ;; (org-date :foreground orange)
   ;; (org-code :foreground dark-blue)
   ;; (org-special-keyword :foreground base8 :underline t)
   ;; (org-document-title :foreground base8 :weight 'bold :height 1.5)
   ;; (org-document-info-keyword :foreground base4 :height 0.75)
   ;; (org-block-begin-line :foreground base4 :height 0.65)
   ;; (org-meta-line :foreground base4 :height 0.65)
   ;; (org-list-dt :foreground magenta)

   ;; (org-todo-keyword-faces
   ;;  '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
   ;;    ("WAITING" :foreground "#9f7efe" :weight normal :underline t)
   ;;    ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
   ;;    ("DONE" :foreground "#50a14f" :weight normal :underline t)
   ;;    ("CANCELLED" :foreground "#ff6480" :weight normal :underline t)))

   ;; (org-priority-faces '((65 :foreground "#e45649")
   ;;                       (66 :foreground "#da8548")
   ;;                       (67 :foreground "#0098dd")))

   (helm-candidate-number :background blue :foreground bg)

   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))))
