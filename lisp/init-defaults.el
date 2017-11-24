;; init-defaults.el

(defun indent-buffer ()
  "Indent the current visited buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buyffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region"))
      (progn
	(indent-buffer)
	(message "Indented buffer.")))))

(setq ring-bell-function 'ignore)

(global-auto-revert-mode t)

(global-linum-mode t)

(add-hook 'emacs-lisp-mode-hook (lambda () (abbrev-mode t)))
(define-abbrev-table 'global-abbrev-table '(
					    ;; signature
					    ("z4d" "zero4drift")
					    ))

(setq make-backup-files nil)
(setq auto-save-default nil)

(recentf-mode 1)            
(setq recentf-max-menu-items 25)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(global-hl-line-mode t)

(electric-indent-mode t)

(delete-selection-mode t)

(provide 'init-defaults)
