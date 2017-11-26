;; init-elpa.el

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/") t)
  )

(require 'cl)

;; add what ever packages you want here
(defvar zero4drift-packages '(
			      counsel
			      company
			      expand-region
			      iedit
			      magit
			      org-pomodoro
			      popwin
			      smartparens
			      swiper
			      hungry-delete
			      solarized-theme
			      ) "Default packages")

(setq package-selected-packages zero4drift-packages)

(defun zero4drift-packages-installed-p ()
  (loop for pkg in zero4drift-packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (zero4drift-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg zero4drift-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'init-elpa)
