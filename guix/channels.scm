(use-modules (guix ci))

(list
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit "a96e2451bda5aaf9b48339edee392c6a3017d730")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

 (channel
  (name 'jazacash)
  (url  "git@github.com:jazafund/jazacash.git")
  (branch "develop"))

 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (commit "e12455a5e8d524c5c3fba1ae56ebc6819b4f9320")
  (branch "master")))
