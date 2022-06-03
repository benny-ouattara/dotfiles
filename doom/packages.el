;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-super-agenda)
(package! org-bullets)
(package! org-fancy-priorities)
(package! modus-themes)
(package! rfc-mode)
(package! lsp-java)
(package! leetcode)
(package! org-mime)
(package! pass)
(package! dired-single)
(package! rainbow-mode)
(package! mvn)
(package! skeletor)
(package! s)
(package! f)
(package! nano-doom
  :recipe (:host github :repo "skyler544/doom-nano-testing"))
;; temporary fix to map.el bug
(package! map :pin "bb50dba")
;;(package! geiser-guile)
;;(package! guix)
