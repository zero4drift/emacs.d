;; init.el

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list
    'load-path 
    (expand-file-name "lisp" user-emacs-directory))

(add-to-list
	'custom-theme-load-path
	(expand-file-name
		"lisp/custom-themes/solarized"
		user-emacs-directory))
	
(require 'init-elpa)
(require 'init-company)
(require 'init-magit)
(require 'init-keybindings)
(require 'init-ui)
(require 'init-defaults)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
