#!/usr/bin/env bash
set -e
cd "$(git rev-parse --show-toplevel)"

echo "🔍 Validating Otter-Ops Fleet..."

# Validate GNU Guix (Scheme): read every form with Guix's reader, which
# understands gexp syntax (#~, #$) that plain guile rejects
if command -v guix >/dev/null; then
    echo "Checking Scheme syntax..."
    mapfile -d '' files < <(git ls-files -z '*.scm')
    guix repl -- /dev/stdin "${files[@]}" <<'EOF'
(use-modules (guix gexp))
(define bad 0)
(for-each (lambda (file)
            (catch #t
              (lambda ()
                (call-with-input-file file
                  (lambda (port)
                    (let loop ()
                      (unless (eof-object? (read port))
                        (loop))))))
              (lambda (key . args)
                (set! bad (1+ bad))
                (format (current-error-port) "FAIL ~a: ~a ~s~%" file key args))))
          (cdr (command-line)))
(exit (zero? bad))
EOF
fi

echo "✅ All clear! Proceeding with commit."
