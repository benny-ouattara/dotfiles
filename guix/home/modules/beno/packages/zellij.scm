(define-module (beno packages zellij)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public zellij
  (package
    (name "zellij")
    (version "0.45.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/zellij-org/zellij/releases/download/v"
                           version "/zellij-x86_64-unknown-linux-musl.tar.gz"))
       (sha256
        (base32 "1z7f30vlr0wmlmmrj1hmhxqan4pyh5h6g7z3akhfhnjx7zhc5g20"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("zellij" "bin/"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://zellij.dev")
    (synopsis "Terminal workspace and multiplexer")
    (description
     "Zellij is a terminal workspace with panes, tabs, layouts, session
resurrection and a WebAssembly plugin system.  This package installs the
upstream static musl build.")
    (license license:expat)))
