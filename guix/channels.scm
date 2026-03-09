(use-modules (guix ci))

(define %token (getenv "GITHUB_TOKEN"))
(define %repo (string-append "https://jazafund:" %token "@github.com/jazafund/jazacash.git"))

(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
         "1980960f932063f42f97ad3be4b020f68d24e62b")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'jazacash)
        (url %repo)
        (branch "develop"))
      (channel
        (name 'pantherx)
        (url "https://codeberg.org/gofranz/panther.git")
        (introduction
         (make-channel-introduction
          "7e01d7fd21e20f04acbb05c51fbe8b365e70eebd"
          (openpgp-fingerprint
           "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB"))))
      (channel
        (name 'guix)
        (url "https://git.guix.gnu.org/guix.git")
        (branch "master")
        (commit
         "a29122743a67a453ca74042e00d521fffcbc3310")
        (introduction
         (make-channel-introduction
          "9edb3f66fd807b096b48283debdcddccfea34bad"
          (openpgp-fingerprint
           "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
