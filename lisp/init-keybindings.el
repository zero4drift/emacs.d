;; init-keybindgs.el

;; bindings for general utilization

;; shortcut to find function, variable
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; shortcut to open init.el
(global-set-key (kbd "<f12>") 'open-init)

;; shortcut to format buffer or selected region
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; shortcut to auto complete when company not triggered
(global-set-key (kbd "C-c /") 'hippie-expand)

;; bindings for company
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  )

;; bindings for dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; enhancement and binding for occur
(defun occur-dwim ()
  "Call 'occur' with a same default"
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)

;; bindings for iedit
(global-set-key (kbd "C-;") 'iedit-mode)

;; bindings for expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; bindings for org
(global-set-key (kbd "C-c a") 'org-agenda)

;; bindings for swiper
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; bindings for recentf
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; bindings for magit
(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init-keybindings)
