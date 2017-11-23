;; init-ui.el

(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default cursor-type 'bar)

(global-linum-mode t)
(global-hl-line-mode t)

(provide 'init-ui)
