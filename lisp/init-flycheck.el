;; init-flycheck.el


(add-hook 'after-init-hook #'global-flycheck-mode)
(when (not (display-graphic-p))
  (setq flycheck-indication-mode nil))

(provide 'init-flycheck)
