;; init-ledger-mode.el

(require 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(provide 'init-ledger-mode)
