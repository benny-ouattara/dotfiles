;;; tools/wrench/+spotify.el -*- lexical-binding: t; -*-

(load! "sp-helper")

(when (workstation-p)
  (progn
    (require 'sp-hosts)
    (require 'sp-shell)

    (after! tramp  (tramp-set-completion-function "ssh"
                                                  (list (list 'spotify--tramp-user-hosts spotify--hostfile-path))))

    (after! tramp  (tramp-set-completion-function "scp"
                                                  (list (list 'spotify--tramp-user-hosts spotify--hostfile-path))))

    ;; disable company on remote buffers to increase UI response time
    (defun shell-mode-remote-hook ()
      (when (and (fboundp 'company-mode)
                 (file-remote-p default-directory))
        (company-mode -1)))
    (add-hook 'shell-mode-hook 'shell-mode-remote-hook)

    (map! :leader
          (:prefix-map ("o" . "open")
           (:prefix ("s" . "spotify")
            (:prefix ("s" . "shell & storage")
             :desc "url at point" "u" #'spotify-eshell-url-at-point
             :desc "remote shell" "r" #'spotify--remote-shell
             :desc "remote shell command" "c" #'spotify--remote-shell-command
             :desc "postgres" "p" #'sql-connect))))))
