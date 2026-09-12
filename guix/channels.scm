(list (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (branch "master")
       (commit
        "a89286d75f8dcadeabcf807eb259203aff259a63")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      ;; Guix sandboxes channel files; getenv needs --unsafe-channel-evaluation
      (channel
       (name 'jazacash)
       (url (string-append "https://jazafund:" (getenv "GITHUB_TOKEN")
                           "@github.com/jazafund/jazacash.git"))
       (branch "develop"))
      (channel
       (name 'pantherx)
       (url "https://codeberg.org/gofranz/panther.git")
       (branch "master")
       (commit "77ed154bd927c014f8f77d14ba493b41a082f1d8")
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
        "e1f2750b3d265dfb3958ea07c91630b0f9b362c2")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
