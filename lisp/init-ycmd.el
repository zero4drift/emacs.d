;; init-ycmd.el


(require 'ycmd)

(add-hook 'c++-mode-hook 'ycmd-mode)
;; "~/github/ycmd/ycmd" is where my built ycmd package locates on ubuntu.
(set-variable 'ycmd-server-command `("python" ,(file-truename "~/github/ycmd/ycmd/")))
(set-variable 'ycmd-global-config (file-truename "~/github/global-config.py"))

(provide 'init-ycmd)
