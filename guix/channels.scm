(use-modules (guix ci))

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
  (branch "master"))

 (channel
  (name 'todo-releases)
  (url  "git@github.com:benny-ouattara/todo-releases.git")
  ;; (url (string-append "file:///home/ben/Code/todo"))
  (branch "main")))
