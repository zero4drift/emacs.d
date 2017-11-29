
;; init-org

(require 'org)

(setq org-src-fontify-natively t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-agenda-files '("~/org"))

;; org capture
(setq org-capture-templates
      '((
	 "t"
	 "Todo"
	 entry
	 (file+headline "~/org/gtd.org"  "学习安排")
	 "* TODO [#B] %?\n %i\n"
	 :empty-lines 1)))

(provide 'init-org)
