;; init-defaults.el

(setq ring-bell-function 'ignore)

(global-auto-revert-mode t)

(global-linum-mode t)

(abbrev-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(recentf-mode 1)            
(setq recentf-max-menu-items 25)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(global-hl-line-mode t)

(electric-indent-mode t)

(delete-selection-mode t)

(provide 'init-defaults)
