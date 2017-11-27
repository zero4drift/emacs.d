;; init-defaults.el

;; coding-system as utf-8
(setq system-time-locale "en_US")
(set-language-environment "UTF-8")
(setq-default default-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-8)

;; functions defined for indent
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

;; hippie-expand enhanced
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

;; too many typing when emacs asks yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; better defaults for dired
(require 'dired-x)
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(put 'dired-find-alternate-file 'disabled nil)

(setq inhibit-startup-screen -1)

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

;; show parens enhanced
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Hightlight enclosing parens"
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

(global-hl-line-mode t)

(electric-indent-mode t)
(delete-selection-mode t)

(provide 'init-defaults)
