;; init.el

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; 设置垃圾回收，在Windows下，emacs25版本会频繁出发垃圾回收，所以需要设置
(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; 显示垃圾回收信息，这个可以作为调试用
  ;; (setq garbage-collection-messages t)
  )

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

(require 'init-elpa)
(require 'init-company)
(require 'init-company-ycmd)
(require 'init-flycheck)
(require 'init-flycheck-ycmd)
(require 'init-ggtags)
(require 'init-hungry-delete)
(require 'init-ledger-mode)
(require 'init-magit)
(require 'init-org)
(require 'init-popwin)
(require 'init-pyim)
(require 'init-smartparens)
(require 'init-swiper)
(require 'init-ui)
(require 'init-defaults)
(require 'init-wttrin)
(require 'init-which-key)
(require 'init-ycmd)
(require 'init-keybindings)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
