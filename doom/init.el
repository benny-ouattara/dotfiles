;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom install'
;; will do this for you). The `doom!' block below controls what modules are
;; enabled and in what order they will be loaded. Remember to run 'doom refresh'
;; after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

(doom! :input
           ;;chinese
           ;;japanese
           ;;layout            ; auie,ctsrnm is the superior home row

           :completion
           (company
            +childframe)                ; the ultimate code completion backend
           ;;helm              ; the *other* search engine for love and life
           ;;ido               ; the other *other* search engine...
           ;; ivy                              ; a search engine for love and life
           vertico

           :ui
           ;;deft              ; notational velocity for Emacs
           doom                         ; what makes DOOM look the way it does
           doom-dashboard               ; a nifty splash screen for Emacs
           ;; doom-quit         ; DOOM quit-message prompts when you quit Emacs
           ;;fill-column       ; a `fill-column' indicator
           hl-todo            ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
           (hydra +hydra/window-nav/body
                  +hydra/text-zoom/body)
           (emoji +unicode)  ; 🙂
           ;;indent-guides     ; highlighted indent columns
           ;;  (modeline +light)          ; snazzy, Atom-inspired modeline, plus API
           nav-flash                  ; blink the current line after jumping
           ;;neotree           ; a project drawer, like NERDTree for vim
           ophints                ; highlight the region an operation acts on
           (popup                 ; tame sudden yet inevitable temporary windows
            +all                  ; catch all popups that start with an asterix
            +defaults)            ; default popup rules
           ;;pretty-code       ; replace bits of code with pretty symbols
           ;;ligatures         ; ligatures and symbols to make your code pretty again
           ;;tabs              ; an tab bar for Emacs
           treemacs                  ; a project drawer, like neotree but cooler
           ;;unicode           ; extended unicode support for various languages
           vc-gutter          ; vcs diff in the fringe
           vi-tilde-fringe    ; fringe tildes to mark beyond EOB
           window-select      ; visually switch windows
           workspaces         ; tab emulation, persistence & separate workspaces
           ;;zen               ; distraction-free coding or writing

           :editor
           (evil +everywhere)           ; come to the dark side, we have cookies
           file-templates               ; auto-snippets for empty files
           ;; fold                           ; (nigh) universal code folding
           ;;(format +onsave)  ; automated prettiness
           ;;god               ; run Emacs commands without modifier keys
           lispy                   ; vim for lisp, for people who don't like vim
           multiple-cursors        ; editing in many places at once
           ;;objed             ; text object editing for the innocent
           ;;parinfer          ; turn lisp into python, sort of
           rotate-text           ; cycle region at point between text candidates
           snippets              ; my elves. They type so I don't have to
           word-wrap             ; soft wrapping with language-aware indent

           :emacs
           dired         ; making dired pretty [functional]
           electric      ; smarter, keyword-based electric-indent
           ibuffer       ; interactive buffer management
           undo          ; persistent, smarter undo for your inevitable mistakes
           vc            ; version-control and Emacs, sitting in a tree

           :term
           eshell               ; a consistent, cross-platform shell (WIP)
           ;; shell             ; a terminal REPL for Emacs
           ;;term              ; terminals in Emacs
           vterm                        ; another terminals in Emacs

           :checkers
           syntax              ; tasing you for every semicolon you forget
           ;; ;;(spell +flyspell) ; tasing you for misspelling mispelling
           ;; ;;grammar           ; tasing grammar mistake every you make

           :tools
           ;;ansible
           ;;biblio            ; Writes a PhD for you (citation needed)
           (debugger +lsp)   ; FIXME stepping through code, to help you add bugs
           direnv
           ;; (hammer +spotify +container)
           docker
           ;;editorconfig      ; let someone else argue about tabs vs spaces
           ;;ein               ; tame Jupyter notebooks with emacs
           (eval +overlay)   ; run code, run (also, repls)
           ;;gist              ; interacting with github gists
           (lookup              ; helps you navigate your code and documentation
            +docsets)           ; ...or in Dash docsets locally
           (lsp +eglot)
           ;;macos             ; MacOS-specific commands
           magit                        ; a git porcelain for Emacs
           make                         ; run make tasks from Emacs
           (pass
            +auth)                      ; password manager for nerds
           pdf                          ; pdf enhancements
           ;;prodigy           ; FIXME managing external services & code builders
           ;;rgb               ; creating color strings
           ;;terraform         ; infrastructure as code
           tmux                       ; an API for interacting with tmux
           tree-sitter                ; syntax and parsing, sitting in a tree...
           ;;upload            ; map local to remote projects via ssh/ftp
           ;;wakatime

           :os
           ;; (:if IS-MAC macos)  ; improve compatibility with macOS
           ;;tty               ; improve the terminal Emacs experience

           :lang
           ;;agda              ; types of types of types of types...
           ;;assembly          ; assembly for fun or debugging
           ;;beancount         ; mind the GAAP
           (cc +lsp)     ; C/C++/Obj-C madness
           clojure       ; java with a lisp
           common-lisp   ; if you've seen one lisp, you've seen them all
           ;;coq               ; proofs-as-programs
           ;;crystal           ; ruby at the speed of c
           ;;csharp            ; unity, .NET, and mono shenanigans
           data                 ; config/data formats
           ;;(dart +flutter)   ; paint ui and not much else
           ;;dhall
           ;;elixir            ; erlang done right
           ;;elm               ; care for a cup of TEA?
           emacs-lisp                   ; drown in parentheses
           ;;erlang            ; an elegant language for a more civilized age
           ;;ess               ; emacs speaks statistics
           ;;factor
           ;;faust             ; dsp, but you get to keep your soul
           ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
           ;;fsharp           ; ML stands for Microsoft's Language
           ;;fstar             ; (dependent) types and (monadic) effects and Z3
           ;;gdscript          ; the language you waited for
           ;;(go +lsp)         ; the hipster dialect
           ;;(graphql +lsp)    ; Give queries a REST
           ;;(haskell +lsp)    ; a language that's lazier than I am
           ;;hy                ; readability of scheme w/ speed of python
           ;;idris             ;
           json                         ; At least it ain't XML
           (java +lsp
                 +tree-sitter)     ; the poster child for carpal tunnel syndrome
           ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
           ;;julia             ; a better, faster MATLAB
           ;;kotlin            ; a better, slicker Java(Script)
           ;;latex             ; writing papers in Emacs has never been so fun
           ;;lean
           ;;ledger            ; an accounting system in Emacs
           ;;lua               ; one-based indices? one-based indices
           markdown      ; writing docs for people to ignore
           ;;nim               ; python + lisp at the speed of c
           nix           ; I hereby declare "nix geht mehr!"
           ;;ocaml             ; an objective camel
           (org          ; organize your plain life in plain text
            +roam2       ; TODO: upgrade to +roam2
            +dragndrop   ; drag & drop files/images into org buffers
            ;;+hugo            ; use Emacs for hugo blogging
            +ipython     ; ipython/jupyter support for babel
            +pandoc      ; export-with-pandoc support
            ;; +pomodoro    ; be fruitful with the tomato technique
            +noter
            +present
            +pretty
            +journal)                   ; using org-mode for presentations
           ;;perl              ; write code no one else can comprehend
           ;;php               ; perl's insecure younger brother
           ;;plantuml          ; diagrams for confusing people more
           ;;purescript        ; javascript, but functional
           (python +lsp
                   +tree-sitter)       ; beautiful is better than ugly
           ;;qt                ; the 'cutest' gui framework ever
           ;;racket            ; a DSL for DSLs
           ;;raku              ; the artist formerly known as perl6
           rest                         ; Emacs as a REST client
           ;;rst               ; ReST in peace
           (ruby +rbenv
                 +rails
                 +tree-sitter) ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
           ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
           (scala +lsp
                  +tree-sitter)      ; java, but good
           (scheme +guile)           ; a fully conniving family of lisps
           sh                        ; she sells {ba,z,fi}sh shells on the C xor
           ;;sml
           ;;solidity          ; do you need a blockchain? No.
           ;;swift             ; who asked for emoji variables?
           ;;terra             ; Earth and Moon in alignment for performance.
           ;;web               ; the tubes
           yaml                         ; JSON, but readable
           ;;zig               ; C, but simpler

           :email
           (mu4e +gmail)
           ;; (notmuch +afew +org)
           ;;(wanderlust +gmail)

           ;; Applications are complex and opinionated modules that transform Emacs
           ;; toward a specific purpose. They may have additional dependencies and
           ;; should be loaded late.
           :app
           calendar
           ;;emms
           ;;everywhere        ; *leave* Emacs!? You must be joking
           ;; irc               ; how neckbeards socialize
           (rss +org)                   ; emacs as an RSS reader
           ;;twitter           ; twitter client https://twitter.com/vnought

           :config
           literate
           (default +bindings +smartparens))
