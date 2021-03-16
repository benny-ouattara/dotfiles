;; -*- no-byte-compile: t; -*-
;;; tools/hammer/packages.el

;; +spotify utils related dependencies
(package! sp-hosts :recipe (:host github :repo "benny-ouattara/sp-hosts.el") :pin "1bfa4db")
(package! sp-shell :recipe (:host github :repo "benny-ouattara/sp-shell.el") :pin "8f8d8aa")
(package! sp-pg :recipe (:host github :repo "benny-ouattara/sp-pg.el") :pin "48fb0a3")

;; +container related dependencies
(package! kubel)
(package! kubel-evil)
