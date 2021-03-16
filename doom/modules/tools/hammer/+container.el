;;; tools/wrench/+container.el -*- lexical-binding: t; -*-

(load! "sp-helper")

(when (workstation-p)
  (progn
    (use-package! kubel)
    (use-package! kubel-evil)

    (map! :leader
          (:prefix-map ("o" . "open")
           (:prefix ("s" . "spotify")
            :desc "kubernetes" "k" #'kubel)))))
