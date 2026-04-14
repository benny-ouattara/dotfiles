;;; init.el -*- lexical-binding: t; -*-
(doom! :completion
       (corfu +orderless)            ; the ultimate code completion backend
       (vertico +childframe)

       :ui
       doom                          ; what makes DOOM look the way it does
       doom-dashboard                ; a nifty splash screen for Emacs
       hl-todo                       ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (hydra +hydra/window-nav/body
              +hydra/text-zoom/body)
       nav-flash                     ; blink the current line after jumping
       ophints                       ; highlight the region an operation acts on
       (popup                        ; tame sudden yet inevitable temporary windows
        +all                         ; catch all popups that start with an asterix
        +defaults)                   ; default popup rules
       treemacs                      ; a project drawer, like neotree but cooler
       vc-gutter                     ; vcs diff in the fringe
       vi-tilde-fringe               ; fringe tildes to mark beyond EOB
       window-select                 ; visually switch windows
       workspaces                    ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)            ; come to the dark side, we have cookies
       file-templates                ; auto-snippets for empty files
       (format +onsave)              ; automated prettiness
       lispy                         ; vim for lisp, for people who don't like vim
       multiple-cursors              ; editing in many places at once
       rotate-text                   ; cycle region at point between text candidates
       snippets                      ; my elves. They type so I don't have to
       word-wrap                     ; soft wrapping with language-aware indent

       :emacs
       dired                         ; making dired pretty [functional]
       electric                      ; smarter, keyword-based electric-indent
       ibuffer                       ; interactive buffer management
       undo                          ; persistent, smarter undo for your inevitable mistakes
       tramp                         ; remote files at your arthritic fingertips
       vc                            ; version-control and Emacs, sitting in a tree

       :term
       eshell                        ; a consistent, cross-platform shell (WIP)
       vterm                         ; another terminals in Emacs

       :checkers
       syntax                        ; tasing you for every semicolon you forget

       :tools
       (debugger +lsp)               ; stepping through code, to help you add bugs
       direnv
       docker
       (eval +overlay)               ; run code, run (also, repls)
       lookup                        ; ...or in Dash docsets locally
       (lsp +eglot)
       llm                           ; when I said you needed friends, I didn't mean...
       magit                         ; a git porcelain for Emacs
       make                          ; run make tasks from Emacs
       (pass +auth)                  ; password manager for nerds
       pdf                           ; pdf enhancements
       tmux                          ; an API for interacting with tmux
       tree-sitter                   ; syntax and parsing, sitting in a tree...

       :os
       (:if (featurep :system 'macos) macos) ; improve compatibility with macOS
       (tty +osc)                    ; improve the terminal Emacs experience

       :lang
       (clojure +lsp)                ; java with a lisp
       common-lisp                   ; if you've seen one lisp, you've seen them all
       data                          ; config/data formats
       emacs-lisp                    ; drown in parentheses
       json                          ; At least it ain't XML
       markdown                      ; writing docs for people to ignore
       nix                           ; I hereby declare "nix geht mehr!"
       (org                          ; organize your plain life in plain text
        +roam                        ; org-roam
        +pandoc                      ; export-with-pandoc support
        +pretty
        +journal)
       (python +lsp +tree-sitter)    ; beautiful is better than ugly
       (rest +jq)                    ; Emacs as a REST client
       (scheme +guile)               ; a fully conniving family of lisps
       sh                            ; she sells {ba,z,fi}sh shells on the C xor
       web                           ; the tubes
       yaml                          ; JSON, but readable

       :email
       (mu4e +gmail)
       ;;(notmuch +afew +org)

       :app
       (rss +org)                    ; emacs as an RSS reader

       :config
       literate
       (default +bindings +smartparens))
