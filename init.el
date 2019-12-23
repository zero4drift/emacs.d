;;; Grouped init.el enhanced by use-package

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)


;; elpa
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives
	'(("melpa" . "http://elpa.emacs-china.org/melpa/")
	  ("gnu" . "http://elpa.emacs-china.org/gnu/")
	  ("org" . "http://elpa.emacs-china.org/org/"))))

;; scheme: delete this after finishing review of SICP Exercises
(setq scheme-program-name "scheme")

;; to estimate start-up time usage
(defconst emacs-start-time (current-time))
(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed (float-time (time-subtract (current-time)
						       emacs-start-time))))
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name elapsed)))
	  t)

;; use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; ui
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default cursor-type 'bar)

;; just do not show the original startup screen
(setq inhibit-startup-screen -1)

;; too many typing when emacs asks yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; make symbols look better
(global-prettify-symbols-mode 1)

;; subty flash the modeline instead of ringbell 'ding'
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line
			       (face-foreground 'error))
          (run-with-idle-timer 0.3 nil
                               (lambda (fg)
				 (set-face-foreground 'mode-line fg))
                               orig-fg))))
;; https://blog.csdn.net/xh_acmagic/article/details/78939246
(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Inconsolata" 18)) ;; 11 13 17 19 23
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Sarasa Mono SC")))) ;; 14 16 20 22 28
    ))

(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))
;; end ui


;; functions and related keybindings
;; enhancement and binding for occur
(defun occur-dwim ()
  "Call 'occur' with a same default."
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

;; bindings for recentf
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; shortcut to open init.el
(defun open-init()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "<f4>") 'open-init)

;; shortcut to find function, variable
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; functions defined for indent
(defun indent-buffer ()
  "Indent the current visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buyffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region"))
      (progn
	(indent-buffer)
	(message "Indented buffer.")))))

;; shortcut to format buffer or selected region
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)


;; files, folders, buffers and text
;; use fundamental-mode to open large files
(defun zero4drift-check-large-file ()
  (when (> (buffer-size) 500000)
    (progn (fundamental-mode)
	   (hl-line-mode -1))))
(add-hook 'find-file-hook 'zero4drift-check-large-file)

;; when the file is modified, the related buffer will change.
(global-auto-revert-mode t)

;; do not make backup files
(setq make-backup-files nil)

;; do not auto save
(setq auto-save-default nil)

;; length of one line should not exceed 80 characters
(setq-default fill-column 80)

;; active recentf mode and set the max menu items
(recentf-mode 1)
(setq-default recentf-max-menu-items 25)

;; encoding system
(setq-default language-environment 'utf-8)

;; show parens enhanced
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Hightlight enclosing parens"
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

;; on-the-fly indentation
(electric-indent-mode t)

;; typed text replaces the selection
(delete-selection-mode t)

;; end functions and related keybindings


;; use packages
;; org
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; allow e-lisp code evaluated in org files
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
			       (ledger . t)))
  (org-src-fontify-natively t)
  (org-clock-into-drawer t)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-log-state-notes-insert-after-drawers nil)
  (org-tag-alist (quote (("@errand" . ?e)
			 ("@office" . ?o)
			 ("@home" . ?h)
			 ("@school" . ?s)
			 (:newline)
			 ("WAITING" . ?w)
			 ("HOLD" . ?H)
			 ("CANCELLED" . ?c))))
  (org-fast-tag-selection-single-key nil)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'cofirm)
  (org-refile-targets '(("next.org" :level . 0)
			("someday.org" :level . 0)
			("reading.org" :level . 1)
			("projects.org" :maxlevel . 1)))
  (org-return-follows-link t)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  :custom-face
  (org-done ((t (:strike-through t :weight bold))))
  (org-headline-done ((t (:strike-through t))))
  (org-image-actual-width (/ (display-pixel-width) 2))
  :custom
  (org-structure-template-alist '(("a" . "export ascii")
                                  ("c" . "center")
                                  ("C" . "comment")
                                  ("e" . "example")
                                  ("E" . "export")
                                  ("h" . "export html")
                                  ("l" . "export latex")
                                  ("q" . "quote")
                                  ("s" . "src")
                                  ("v" . "verse")
                                  ("el" . "src emacs-lisp ")
                                  ("d" . "definition")
                                  ("t" . "theorem")))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c b" . org-iswitchb)
   :map org-mode-map
   ("M-n" . outline-next-visible-heading)
   ("M-p" . outline-previous-visible-heading)))

(require 'find-lisp)
(setq zero4drift/org-agenda-directory "~/.org/gtd/")
(setq org-agenda-files (find-lisp-find-files zero4drift/org-agenda-directory "\.org$"))

;; Stage 1: Collecting
(setq org-capture-templates `(("i" "inbox"
			       entry (file "~/.org/gtd/inbox.org")
			       "* TODO %?")
			      ;; ("e" "email"
			      ;; entry (file+headline "~/.org/gtd/emails.org" "Emails")
			      ;; "* TODO [#A] Reply:%a :@home:@school:" :immediate-finish t)
			      ("l" "link" entry (file "~/.org/gtd/inbox.org")
			       "* TODO %(org-cliplink-capture)" :immediate-finish t)
			      ("w" "Weekly Review"
			       entry (file+olp+datetree "~/.org/gtd/reviews.org")
			       (file "~/.org/gtd/templates/weekly_review.org"))
			      ("s" "Snippet"
			       entry (file "~/.org/deft/capture.org")
			       "* Snippet %<%Y-%m-%d %H:%M>\n%?")))

;; Stage 2: Processing
(require 'org-agenda)
(setq zero4drift/org-agenda-inbox-view
      `("i" "Inbox" todo ""
	((org-agenda-files '("~/.org/gtd/inbox.org")))))
(add-to-list 'org-agenda-custom-commands `,zero4drift/org-agenda-inbox-view)
(setq zero4drift/org-agenda-someday-view
      `("s" "Someday" todo ""
	((org-agenda-files '("~/.org/gtd/someday.org")))))
(add-to-list 'org-agenda-custom-commands `,zero4drift/org-agenda-someday-view)
(setq zero4drift/org-agenda-reading-view
      `("r" "Reading" todo ""
        ((org-agenda-files '("~/.org/gtd/reading.org")))))
(add-to-list 'org-agenda-custom-commands `,zero4drift/org-agenda-reading-view)

(defun zero4drift/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (zero4drift/bulk-process-entries))

(defun zero4drift/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun zero4drift/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region)
		(funcall 'zero4drift/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun zero4drift/org-inbox-capture ()
  "Capture a task in agenda mode."
  (interactive)
  (org-capture nil "i"))

(define-key org-agenda-mode-map "i" 'org-agenda-clock-in)
(define-key org-agenda-mode-map "r" 'zero4drift/org-process-inbox)
(define-key org-agenda-mode-map "R" 'org-agenda-refile)
(define-key org-agenda-mode-map "c" 'zero4drift/org-inbox-capture)

(defvar zero4drift/new-project-template
  "
    *Project Purpose/Principles*:

    *Project Outcome*:
    "
  "Project template, inserted when a new project is created")

(defvar zero4drift/is-new-project nil
  "Boolean indicating whether it's during the creation of a new project")

(defun zero4drift/refile-new-child-advice (orig-fun parent-target child)
  (let ((res (funcall orig-fun parent-target child)))
    (save-excursion
      (find-file (nth 1 parent-target))
      (goto-char (org-find-exact-headline-in-buffer child))
      (org-add-note))
    res))

(advice-add 'org-refile-new-child :around #'zero4drift/refile-new-child-advice)

(defun zero4drift/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "NEXT"))

(add-hook 'org-clock-in-hook 'zero4drift/set-todo-state-next 'append)

;; Stage 3: Reviewing
(setq org-agenda-block-separator nil)
(setq org-agenda-start-with-log-mode t)
(setq zero4drift/org-agenda-todo-view
      `(" " "Agenda"
	((agenda ""
		 ((org-agenda-span 'day)
		  (org-deadline-warning-days 365)))
	 (todo "TODO"
	       ((org-agenda-overriding-header "To Refile")
		(org-agenda-files '("~/.org/gtd/inbox.org"))))
	 ;; (todo "TODO"
	 ;; ((org-agenda-overriding-header "Emails")
	 ;; (org-agenda-files '("~/.org/gtd/emails.org"))))
	 (todo "NEXT"
	       ((org-agenda-overriding-header "In Progress")
		(org-agenda-files '("~/.org/gtd/someday.org"
				    "~/.org/gtd/projects.org"
				    "~/.org/gtd/next.org"))
		;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
		))
	 (todo "TODO"
	       ((org-agenda-overriding-header "Projects")
		(org-agenda-files '("~/.org/gtd/projects.org"))))
	 ;; (org-agenda-skip-function #'zero4drift/org-agenda-skip-all-siblings-but-first)))
	 (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files '("~/.org/gtd/next.org"))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	 nil)))

(add-to-list 'org-agenda-custom-commands `,zero4drift/org-agenda-todo-view)

(defun zero4drift/org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (or (org-current-is-todo)
		(not (org-get-scheduled-time (point))))
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
	(when (org-current-is-todo)
	  (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
	  (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(defun zero4drift/switch-to-agenda ()
  (interactive)
  (org-agenda nil " ")
  (delete-other-windows))

(bind-key "<f1>" 'zero4drift/switch-to-agenda)

;; Column View
(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:}
  %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled)
  %DEADLINE(Deadline)")

;; Stage 4: Doing
;; org-pomodoro
(use-package org-pomodoro
  :after org
  :bind
  (:map org-agenda-mode-map
	("I" . org-pomodoro))
  :custom
  (org-pomodoro-format "%s")
  (org-pomodoro-audio-player (executable-find "mpv")))

;; deft
(use-package deft
  :bind
  (("C-c n" . deft))
  :config
  ;; No deft summary
  (defun deft-parse-summary (contents title))
  :custom
  (deft-default-extension "org")
  (deft-directory "~/.org/deft/")
  (deft-recursive t)
  (deft-file-naming-rules '((noslash . "_")))
  (deft-text-mode 'org-mode)
  (deft-use-filter-string-for-filename t)
  (deft-org-mode-title-prefix t)
  (deft-use-filename-as-title t))

;; iedit
(use-package iedit
  :bind
  (("C-:" . iedit-mode)))

;; expand-region
(use-package expand-region
  :bind
  (("C-=". er/expand-region)))

;; company
(use-package company
  :custom
  (company-idle-delay 0.1)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 3)
  :config
  (global-company-mode)
  :bind
  (:map company-active-map
	("M-n" . nil)
	("M-p" . nil)
	("C-n" . #'company-select-next)
	("C-p" . #'company-select-previous)))

;; begins ccls
(use-package lsp-mode
  :commands lsp
  :config
  (set-face-attribute
   'lsp-face-highlight-read nil
   :underline (face-foreground 'warning))
  (set-face-attribute
   'lsp-face-highlight-write nil
   :underline (face-foreground 'error))
  (set-face-attribute
   'lsp-face-highlight-textual nil
   :underline (face-foreground 'success))
  :custom
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after (evil)
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (define-key evil-normal-state-map (kbd "M-.") 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] 'lsp-ui-peek-find-references)
  (define-key evil-normal-state-map (kbd "C-p") 'lsp-ui-peek-jump-forward)
  (define-key evil-normal-state-map (kbd "C-t") 'lsp-ui-peek-jump-backward))

(use-package company-lsp
  :commands company-lsp
  :defer t
  :custom
  (company-quickhelp-delay 1)
  (company-lsp-enable-snippet t)
  (company-lsp-async t)
  (company-lsp-cache-candidates nil)
  (company-lsp-enable-recompletion t)
  :config
  (push 'company-lsp company-backends))

(use-package ccls
  :init
  (defun ccls/callee () (interactive)
	 (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
  (defun ccls/caller () (interactive)
	 (lsp-ui-peek-find-custom "$ccls/call"))
  (defun ccls/vars (kind)
    (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
  (defun ccls/base (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
  (defun ccls/derived (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun ccls/member (kind) (interactive)
	 (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

  ;; References w/ Role::Role
  (defun ccls/references-read () (interactive)
	 (lsp-ui-peek-find-custom
	  "textDocument/references"
	  (plist-put
	   (lsp--text-document-position-params) :role 8)))

  ;; References w/ Role::Write
  (defun ccls/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom
     "textDocument/references"
     (plist-put
      (lsp--text-document-position-params) :role 16)))

  ;; References w/ Role::Dynamic bit (macro expansions)
  (defun ccls/references-macro () (interactive)
	 (lsp-ui-peek-find-custom
	  "textDocument/references"
	  (plist-put
	   (lsp--text-document-position-params) :role 64)))

  ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
  (defun ccls/references-not-call () (interactive)
	 (lsp-ui-peek-find-custom
	  "textDocument/references"
	  (plist-put
	   (lsp--text-document-position-params) :excludeRole 32)))
  ;; ccls/vars ccls/base ccls/derived ccls/members have a parameter while others are interactive.
  ;; (ccls/base 1) direct bases
  ;; (ccls/derived 1) direct derived
  ;; (ccls/member 2) => 2 (Type) => nested classes / types in a namespace
  ;; (ccls/member 3) => 3 (Func) => member functions / functions in a namespace
  ;; (ccls/member 0) => member variables / variables in a namespace
  ;; (ccls/vars 1) => field
  ;; (ccls/vars 2) => local variable
  ;; (ccls/vars 3) => field or local variable. 3 = 1 | 2
  ;; (ccls/vars 4) => parameter
  :custom
  (ccls-executable "~/github/ccls/Release/ccls")
  (ccls-args '("--log-file=/tmp/ccls.log"))
  :hook ((c-mode c++-mode) .
	 (lambda () (require 'ccls) (lsp))))
;; ends ccls

;; yasnippet
(use-package yasnippet-snippets)

(use-package yasnippet
  :after (company yasnippet-snippets)
  :bind
  (("C-c /" . company-yasnippet))
  :config
  (yas-global-mode t))

;; flycheck
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode))

;; hungry-delete
(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

;; ledger-mode
(use-package ledger-mode
  :init
  (defun open-ledger()
    (interactive)
    (find-file "~/.accounting/2019.ledger"))
  :mode "\\.ledger$"
  :bind (([f3] . open-ledger)))

;; magit
(use-package magit
  :bind
  (("C-x g" . magit-status)))

;; popwin
(use-package popwin
  :config (popwin-mode 1))

;; smartparens
(use-package smartparens-config
  :ensure smartparens
  :config
  (progn (show-smartparens-global-mode t))
  :hook
  (prog-mode . turn-on-smartparens-strict-mode))

;; hydra
(use-package hydra
  :defer t)

(defhydra hydra-smartparens (global-map "M-p s" :hint t)
  "
Sexps (quit with _q_, help with _h_)
^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _→_:          slurp forward   _R_: splice
_b_: backward    _←_:          barf forward    _r_: raise
_u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
_d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
_p_: backward ↓
_n_: forward ↑
^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
  ("q" nil)
  ("h" hydra-smartparens/body)
  ;; Wrapping
  ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
  ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
  ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
  ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
  ;; Navigation
  ("f" sp-forward-sexp )
  ("b" sp-backward-sexp)
  ("u" sp-backward-up-sexp)
  ("d" sp-down-sexp)
  ("p" sp-backward-down-sexp)
  ("n" sp-up-sexp)
  ;; Kill/copy
  ("w" sp-copy-sexp)
  ("k" sp-kill-sexp)
  ;; Misc
  ("t" sp-transpose-sexp)
  ("j" sp-join-sexp)
  ("s" sp-split-sexp)
  ("c" sp-convolute-sexp)
  ("i" sp-indent-defun)
  ;; Depth changing
  ("R" sp-splice-sexp)
  ("r" sp-splice-sexp-killing-around)
  ("<up>" sp-splice-sexp-killing-backward)
  ("<down>" sp-splice-sexp-killing-forward)
  ;; Barfing/slurping
  ("<right>" sp-forward-slurp-sexp)
  ("<left>" sp-forward-barf-sexp)
  ("C-<left>" sp-backward-barf-sexp)
  ("C-<right>" sp-backward-slurp-sexp))

;; counsel
(use-package counsel)
;; swiper
(use-package swiper
  :after (counsel)
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  :bind
  (("\C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

;; solarized-theme
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

;; doom-modeline
;; manual run all-the-icons-install-fonts
(use-package doom-modeline
  :defer t
  :config
  (set-face-attribute 'mode-line nil
		      :underline
		      (face-background 'highlight))
  :custom
  (x-underline-at-descent-line t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  :hook (after-init . doom-modeline-init))

;; ace-window
(use-package ace-window
  :defer t
  :custom
  (aw-background nil)
  :bind (("C-x o" . ace-window)))

;; treemacs
(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      t
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 44)
    
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	([f2] . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; which-key
(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom))

;; pyim Chinese input method
(use-package pyim
  :after (ivy)
  :demand t
  :custom
  (pyim-dicts
   '((:name "CS" :file "~/.emacs.d/pyim/cs.pyim")
     (:name "BigDict" :file "~/.emacs.d/pyim/pyim-bigdict.pyim.gz")))
  :config
  ;; 让swiper 支持 pyim, 搜索词加前缀 .
  (defun eh-ivy-cregexp(str)
    (if (string-match-p "^\\." str)
	(pyim-cregexp-build(substring str 1))
      (ivy--regex-plus str)))

  (setq ivy-re-builders-alist
	'((t . eh-ivy-cregexp)))

  (setq default-input-method "pyim")

  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
		'(pyim-probe-dynamic-english
		  pyim-probe-isearch-mode
		  pyim-probe-program-mode
		  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
		'(pyim-probe-punctuation-line-beginning
		  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (use-package posframe)
  (setq pyim-page-tooltip 'posframe)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
	    #'(lambda () (pyim-restart-1 t)))
  :bind
  (;与 pyim-probe-dynamic-english 配合
   ("M-j" . pyim-convert-string-at-point)))

;; evil-nerd-commenter
(use-package evil-nerd-commenter
  :defer t)

;; begin evil conf
;; evil-leader
(use-package evil-leader
  :hook
  (evil-local-mode . evil-leader-mode)
  :config
  (evil-leader/set-key
    "ff" 'find-file
    "bb" 'switch-to-buffer
    ":"  'counsel-M-x
    ;; evil-nerd-commenter
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-copy-and-comment-operator
    "\\" 'evilnc-comment-operator ; if you prefer backslash key
    ))

;; evil-surround
(use-package evil-surround
  :hook
  (evil-local-mode . turn-on-evil-surround-mode)
  (c++-mode .
	    (lambda ()
	      (push '(?< . ("<" . ">"))
		    evil-surround-pairs-alist)))
  (emacs-lisp-mode .
		   (lambda ()
                     (push '(?` . ("`" . "'"))
			   evil-surround-pairs-alist))))

;; evil-mode
(use-package evil
  :bind
  (([f5] . evil-local-mode))
  :hook ((prog-mode fundamental-mode text-mode)
	 . evil-local-mode))
;; end evil confs

;; youdao-dictionary
(use-package youdao-dictionary
  :custom
  (url-automatic-caching t)
  (youdao-dictionary-search-histroy-file "~/.emacs.d/.youdao")
  :bind
  (("C-c y" . 'youdao-dictionary-search-at-point+)
   ("C-c p" . 'youdao-dictionary-play-voice-at-point)))

;; built-in display-line-mode
(use-package display-line-numbers
  :custom
  (display-line-numbers-width 2)
  (display-line-numbers-grow-only t)
  :init
  (set-face-font 'line-number "Monaco")
  (set-face-background 'line-number-current-line
		       (face-foreground 'success))
  :hook
  ((prog-mode text-mode) . #'display-line-numbers-mode))

;; dired+ make dired human-friendly
;; built-in dired
(setq dired-listing-switches "-alh")
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)

;; blog
(use-package ox-hugo
  :after ox
  :custom
  (org-hugo-section "post"))
(use-package easy-hugo
  :custom
  (easy-hugo-basedir "~/github/blog/"))
;; end blog

;; end use-packages


(provide 'init)
;;; init.el ends here
