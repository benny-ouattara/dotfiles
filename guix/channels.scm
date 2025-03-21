(use-modules (guix ci))

(list
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit "831f3ff14260e20d4da31b707515891eeb49e752") ;; pin commit to avoid unbound variable bug
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
  (commit "5217ea6d45bef053844d8360a06252b9436783b3")
  (branch "master")))
