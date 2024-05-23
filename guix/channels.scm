(use-modules (guix ci))

(define %guix-channel-introduction
  ;; Introduction of the official 'guix channel.  The chosen commit is the
  ;; first one that introduces '.guix-authorizations' on the 'staging'
  ;; branch that was eventually merged in 'master'.  Any branch starting
  ;; before that commit cannot be merged or it will be rejected by 'guix pull'
  ;; & co.
  (make-channel-introduction
   "9edb3f66fd807b096b48283debdcddccfea34bad"     ;2020-05-26
   (openpgp-fingerprint                           ;mbakke
    "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))

(list
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit "d44f0cde50ca96eabf6e7b0f0c9bdbd89508b790") ;; pin commit to avoid unbound variable bug
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (commit "be6f5edd445850720dfcec2642db643b84fc0645")
  (branch "master"))

 ;; (channel
 ;;  (name 'guix)
 ;;  (branch "develop")
 ;;  (url "git@github.com:benny-ouattara/octo-guix.git")
 ;;  (introduction %guix-channel-introduction))

 ;; (channel
 ;;  (name 'contactapp)
 ;;  (url  "git@github.com:benny-ouattara/contactapp.git")
 ;;  (branch "packaging"))
 )
