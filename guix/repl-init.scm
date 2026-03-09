(use-modules
 (guix)
 (gnu system)
 (guix store)
 (guix monads)
 (srfi srfi-1)
 (gnu packages base)
 (ice-9 readline))

(cond ((false-if-exception (resolve-interface '(ice-9 readline)))
       =>
       (lambda (module)
         ;; Enable completion and input history at the REPL.
         ((module-ref module 'activate-readline))))
      (else
       (display "Consider installing the 'guile-readline' package for
convenient interactive line editing and input history.\n\n")))

      (unless (getenv "INSIDE_EMACS")
        (cond ((false-if-exception (resolve-interface '(ice-9 colorized)))
               =>
               (lambda (module)
                 ;; Enable completion and input history at the REPL.
                 ((module-ref module 'activate-colorized))))
              (else
               (display "Consider installing the 'guile-colorized' package
for a colorful Guile experience.\n\n"))))

;; Automatically load your local module path
(let* ((system-mod (string-append (getcwd) "/system/modules"))
      (home-mod (string-append (getcwd) "/home/modules")))
  (set! %load-path (cons* system-mod home-mod %load-path)))

(display "--- Otter-System REPL Initialized ---\n")
(display "Custom modules loaded from ./system/modules and ./home/modules\n")
