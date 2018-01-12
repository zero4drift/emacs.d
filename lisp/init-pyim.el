;; init-pyim.el

(require 'pyim)
(require 'pyim-basedict)

(pyim-basedict-enable)
(setq default-input-method "pyim")
(setq pyim-default-scheme 'quanpin)

(setq-default pyim-english-input-switch-functions
	      '(pyim-probe-dynamic-english
		pyim-probe-isearch-mode
		pyim-probe-program-mode
		pyim-probe-org-structure-template))
(setq-default pyim-punctuation-half-width-functions
	      '(pyim-probe-punctuation-line-beginning
		pyim-probe-punctuation-after-punctuation))

(pyim-isearch-mode 1)
(setq pyim-page-tooltip 'popup)
(setq pyim-page-length 5)
(add-hook 'emacs-startup-hook
	  #'(lambda() (pyim-restart-1 t)))

(provide 'init-pyim)
