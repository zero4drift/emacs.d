;; init-defaults.el

;; modeline
(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
	      'display `((space :align-to
				(- (+ right right-fringe right-margin) ,reserve)))
	      'face face))

(defun buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
	(match-string 1 buf-coding)
      buf-coding)))

(setq my-flycheck-mode-line
      '(:eval
	(pcase flycheck-last-status-change
	  (`not-checked nil)
	  (`no-checker (propertize " -" 'face 'warning))
	  (`running (propertize " ✷" 'face 'success))
	  (`errored (propertize " !" 'face 'error))
	  (`finished
	   (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
		  (no-errors (cdr (assq 'error error-counts)))
		  (no-warnings (cdr (assq 'warning error-counts)))
		  (face (cond (no-errors 'error)
			      (no-warnings 'warning)
			      (t 'success))))
	     (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
			 'face face)))
	  (`interrupted " -")
	  (`suspicious '(propertize " ?" 'face 'warning)))))

(setq-default mode-line-format
	      (list
	       "%1 "
	       ;; the buffer name; the file name as a tool tip
	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				   'help-echo (buffer-file-name)))
	       
	       " [" ;; insert vs overwrite mode, input-method in a tooltip
	       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
				   'face 'font-lock-preprocessor-face
				   'help-echo (concat "Buffer is in "
						      (if overwrite-mode
							  "overwrite"
							"insert") " mode")))

	       ;; was this buffer modified since the last save?
	       '(:eval (when (buffer-modified-p)
			 (concat ","  (propertize "Mod"
						  'face 'font-lock-warning-face
						  'help-echo "Buffer has been modified"))))

	       ;; is this buffer read-only?
	       '(:eval (when buffer-read-only
			 (concat ","  (propertize "RO"
						  'face 'font-lock-type-face
						  'help-echo "Buffer is read-only"))))
	       "] "

	       ;; relative position, size of file
	       "["
	       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
	       "/"
	       (propertize "%I" 'face 'font-lock-constant-face) ;; size
	       "] "

	       ;; the current major mode for the buffer.
	       '(:eval (propertize "%m" 'face 'font-lock-string-face
				   'help-echo buffer-file-coding-system))

	       "%1 "
	       my-flycheck-mode-line
	       "%1 "

	       ;; minor modes but I feel that's too many
	       ;; minor-mode-alist
	       
	       " "
	       ;; git info
	       `(vc-mode vc-mode)

	       " "
	       
	       ;; global-mode-string goes in mode-line-misc-info
	       mode-line-misc-info

	       (mode-line-fill 'mode-line 20)

	       ;; line and column
	       "(" ;; '%02' to set to 2 chars at least; prevents flickering
	       (propertize "%02l" 'face 'font-lock-type-face) ","
	       (propertize "%02c" 'face 'font-lock-type-face)
	       ") "

	       '(:eval (buffer-encoding-abbrev))
	       mode-line-end-spaces " "
	       ;; add the time, with the date and the emacs uptime in the tooltip
	       '(:eval (propertize (format-time-string "%H:%M")
				   'help-echo
				   (concat (format-time-string "%c; ")
					   (emacs-uptime "Uptime:%hh"))))
	       ))

;; make symbols look better
(global-prettify-symbols-mode 1)

;; use fundamental-mode to open large files
(defun zero4drift-check-large-file ()
  (when (> (buffer-size) 500000)
    (progn (fundamental-mode)
	   (hl-line-mode -1))))
(add-hook 'find-file-hook 'zero4drift-check-large-file)

;; encoding system
(setq-default language-environment 'utf-8)

;; functions defined for indent
(defun indent-buffer ()
  "Indent the current visited buffer."
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

;; just do not show the original startup screen
(setq inhibit-startup-screen -1)

;; ignore the ring bell
(setq ring-bell-function 'ignore)

;; when the file is modified, the related buffer will change.
(global-auto-revert-mode t)

;; associate e-lisp-mode with abbrev-mode, define an abbrev
(add-hook 'emacs-lisp-mode-hook (lambda () (abbrev-mode t)))
(define-abbrev-table 'global-abbrev-table '(
					    ;; signature
					    ("z4d" "zero4drift")
					    ))

;; do not make backup files
(setq make-backup-files nil)

;; do not auto save
(setq auto-save-default nil)

;; length of one line should not exceed 80 characters
(setq-default fill-column 80)

;; active recentf mode and set the max menu items
(recentf-mode 1)
(setq-default recentf-max-menu-items 25)

;; show parens enhanced
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Hightlight enclosing parens"
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

;; on-the-fly indentation
(electric-indent-mode t)

;; typed text replaces the selection
(delete-selection-mode t)

(provide 'init-defaults)
