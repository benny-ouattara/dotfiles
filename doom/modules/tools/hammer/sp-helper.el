;;; tools/hammer/sp-helper.el -*- lexical-binding: t; -*-

(defconst workstation "zangao")
(defconst station  (if (equal workstation (user-login-name))
                       'work-station
                     'personal-station))

(defun workstation-p ()
  "Predicate that determines if instance is running on work station or personal station."
  (equal station 'work-station))

(provide 'sp-helper)
