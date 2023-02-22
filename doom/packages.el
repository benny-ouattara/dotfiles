;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-super-agenda)
(package! org-bullets)
(package! modus-themes)
(package! rfc-mode)
(package! lsp-java)
(package! leetcode)
(package! org-mime)
(package! pass)
(package! dired-single)
(package! rainbow-mode)
(package! mvn)
;; (package! skeletor)
(package! s)
(package! f)
;; (package! nano-doom
;;   :recipe (:host github :repo "skyler544/doom-nano-testing"))
;; temporary fix to map.el bug
;; (package! map :pin "bb50dba")
;;(package! geiser-guile)
;;(package! guix)
(package! keyfreq)
(package! protobuf-mode)
(package! vertico-posframe :recipe (:host github :repo "tumashu/vertico-posframe"))
(package! interaction-log :recipe (:host github :repo "michael-heerdegen/interaction-log.el"))
(package! guix :recipe (:host github :repo "alezost/guix.el"))
(package! info-colors)
(package! org-modern :recipe (:host github :repo "minad/org-modern"))
;; (package! consult :pin "17de436c58df1f0e2036b7cc590b74dcbdefd393")
;; there is a bug where consult depends on compat but doom doesn't have the latest compat library
