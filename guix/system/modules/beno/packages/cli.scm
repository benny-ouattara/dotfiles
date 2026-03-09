(define-module (beno packages cli)
  #:use-module (guix packages)             ; For 'package', 'name', 'version'
  #:use-module (guix download)             ; For 'source' (even if #f)
  #:use-module (guix build-system trivial) ; For 'trivial-build-system'
  #:use-module (gnu packages bash)         ; For 'bash-minimal'
  #:use-module (guix gexp)                 ; For #~ and #$
  #:use-module (gnu packages base)         ; For 'make'
  #:use-module (gnu packages))

(define-public otter-cli
  (package
    (name "up")
    (version "1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils)) 
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (script (string-append bin "/up"))
                 (manifest "/home/ben/Code/dotfiles/guix/guix.scm")
                 (makefile "/home/ben/Code/dotfiles/guix/Makefile"))
            (mkdir-p bin)
            (call-with-output-file script
              (lambda (port)
                ;; We wrap the make command in 'guix shell' using the manifest
                (format port "#!~a/bin/bash~%~%guix shell -m ~a -- make -f ~a \"$@\"~%"
                        #$(specification->package "bash-minimal")
                        manifest
                        makefile)))
            (chmod script #o555)))))
    (inputs (list (specification->package "guix"))) ; Ensure guix is available
    (home-page #f)
    (synopsis "Context-aware wrapper for Guix dotfiles")
    (description "Runs Makefile targets inside a dedicated Guix shell environment.")
    (license #f)))
