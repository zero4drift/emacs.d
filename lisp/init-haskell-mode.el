;; init-haskell-mode.el

(require-package 'haskell-mode)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-to-list 'Info-default-directory-list "~/elpa/haskell-mode/")

(provide 'init-haskell-mode)