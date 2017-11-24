;; init.el

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list
    'load-path 
    (expand-file-name "lisp" user-emacs-directory))
	
(require 'init-elpa)
(require 'init-company)
(require 'init-hungry-delete)
(require 'init-magit)
(require 'init-keybindings)
(require 'init-org)
(require 'init-popwin)
(require 'init-smartparens)
(require 'init-swiper)
(require 'init-ui)
(require 'init-defaults)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
