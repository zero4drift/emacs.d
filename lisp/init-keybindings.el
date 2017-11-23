;; init-keybindgs.el

;; bindings for general utilization

;; shortcut to open init.el
(defun open-init()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init)

;; bindings for recentf
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; bindings for magit
(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init-keybindings)
