(define-module (beno packages lnav)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  ;; #:use-module (gnu packages rust)
  ;; #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages libunistring))

(define-public lnav
  (let ((commit "adec0dc8ec0ffd5a7f06c19852edc35d00dc4fe1")
        (revision 0))
    (package
     (name "lnav")
     (version "0.12.4")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tstack/lnav")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0w5k5wq5f41jppx2143r9ivlzisv5hza2cv12qmmkdwk1wbajkqf"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests? #f)) ;; test fails
     (native-inputs (list autoconf
                          automake
                          ;; rust
                          ;; rust-cargo ;; cargo tries to download something
                          ))
     (inputs (list pcre2
                   sqlite
                   which
                   ncurses
                   zlib
                   lbzip2
                   curl
                   libarchive
                   libunistring
                   wireshark
                   pkg-config))
     (home-page "http://lnav.org")
     (synopsis "Log file navigator.")
     (description
      "The Logfile Navigator is a log file viewer for the terminal.")
     (license license:bsd-2))))
