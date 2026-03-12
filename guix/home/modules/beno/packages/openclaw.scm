(define-module (beno packages openclaw)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu home services)            ; Required for service-type
  #:use-module (gnu home services shepherd)   ; Required for home-shepherd-service-type
  #:use-module (gnu packages containers)      ; for podman
  #:use-module (gnu packages admin)           ; for sudo
  #:use-module (gnu packages version-control) ; for git
  #:use-module (gnu packages base)            ; for tail
  #:export (openclaw-scripts
            home-openclaw-service-type))

(define upgrade-openclaw
  (program-file "upgrade-openclaw"
                #~(begin
                    (use-modules (ice-9 match)
                                 (ice-9 ftw))

                    ;; Define local aliases for store paths to avoid shadowing package names
                    (let ((podman-bin #$(file-append podman "/bin/podman"))
                          (git-bin    #$(file-append git "/bin/git"))
                          ;; Note: sudo must use the setuid path to actually work
                          (sudo-bin   "/run/setuid-programs/sudo")
                          (openclaw-user "openclaw")
                          (repo-url "https://github.com/OpenClaw/OpenClaw.git")
                          (work-dir "/tmp/openclaw-build-space")
                          (img-archive "/tmp/openclaw-transfer.tar"))

                      (define args (command-line))
                      (define run-wizard? (member "onboard" args))

                      (display "🛠️  Preparing OpenClaw for Guix Store...\n")

                      ;; 1. Setup Build Space
                      (when (file-exists? work-dir) (system* "rm" "-rf" work-dir))
                      
                      ;; 2. Clone Repository
                      (display "📥 Cloning repository...\n")
                      (unless (zero? (system* git-bin "clone" "--depth" "1" repo-url work-dir))
                        (error "Failed to clone repository."))

                      (chdir work-dir)

                      ;; 3. Build Image
                      (display "📦 Building Podman image...\n")
                      (unless (zero? (system* podman-bin "build" "-t" "openclaw:local" "."))
                        (error "Podman build failed."))

                      ;; 4. Export for the openclaw user
                      (display "💾 Exporting image archive...\n")
                      (system* podman-bin "save" "openclaw:local" "-o" img-archive)
                      (chmod img-archive #o644)

                      ;; 5. Load into User Namespace
                      (display (string-append "🚚 Loading into " openclaw-user " namespace...\n"))
                      (unless (zero? (system* sudo-bin "-u" openclaw-user "sh" "-c" 
                                              (string-append podman-bin " load -i " img-archive)))
                        (error "Image load failed."))

                      ;; 6. Cleanup Host Artifacts
                      (system* "rm" "-f" img-archive)
                      (system* "rm" "-rf" work-dir)

                      ;; 7. Launch Wizard
                      (if run-wizard?
                          (begin
                            (display "🧙 Launching OpenClaw Setup...\n")
                            (let ((wizard-cmd (string-append podman-bin " run --rm -v /home/openclaw/.openclaw:/home/node/.openclaw:rw,z -it openclaw:local node dist/index.js onboard")))
                              (system* sudo-bin "-i" "-u" openclaw-user "sh" "-c" wizard-cmd)))
                          (display "✅ Image loaded. Run with 'onboard' argument to launch wizard.\n"))

                      ;; 8. Prune dangling images
                      (display "🧙 Pruning dangling images...\n")
                      (let ((prune-cmd (string-append podman-bin " image prune -f")))
                        (system* sudo-bin "-i" "-u" openclaw-user "sh" "-c" prune-cmd))

                      (display "\n✅ Openclaw latest image built and loaded.\n")))))

(define-public openclaw-scripts
  (package
    (name "openclaw-scripts")
    (version "1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((bin (string-append #$output "/bin")))
            (mkdir-p bin)
            (copy-file #$upgrade-openclaw (string-append bin "/upgrade-openclaw"))
            (chmod (string-append bin "/upgrade-openclaw") #o555)))))
    (inputs
     (list git podman sudo))
    (home-page "https://github.com/OpenClaw/OpenClaw")
    (synopsis "Automation for OpenClaw")
    (description "Managed script for cloning and building OpenClaw via Podman.")
    (license #f)))

