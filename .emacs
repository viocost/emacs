(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup


(require 'package)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize) ; guess what this one does ?

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package) 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (term+ terminal-toggle dumb-jump prettier-js web-mode prettier-js-mode engine-mode htmlize gist slime eval-in-repl eval-in-repl-python exec-path-from-shell jedi epc org-bullets all-the-icons auto-complete flymd markdown-mode tern-auto-complete ctags js-doc doom-modeline spaceline js2-refactor xref-js2 js2-refactpr helm company-tern rainbow-delimiters org-super-agenda treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs ranger js2-mode js2 tern tern-mode autopair evil ivy elpy which-key neotree alpha spacemacs-theme nlinum-relative material-theme evil-mode avy general use-package)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Never type yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; for searching selected in browser
(use-package engine-mode :ensure t)
(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")
(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(use-package general :ensure t)

(toggle-scroll-bar -1)
(set-default-font "Source Code Pro 13")

;; org mode
(use-package org-bullets :ensure t
  :config
  (add-hook 'org-mode 'org-bullets))

(use-package htmlize :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages '((python lisp emacs-lisp C . t)))


(general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")


(use-package ctags :ensure t)

(use-package avy :ensure t)

(use-package evil :ensure t)
(evil-mode 1)
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))

(use-package avy :ensure t
  :commands (avy-goto-word-1))

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;;line numbers
(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-type 'relative
  display-line-numbers-width-start t))
(global-display-line-numbers-mode)

(use-package alpha :ensure t)
(global-set-key (kbd "C-)") 'transparency-decrease)
(global-set-key (kbd "C-(") 'transparency-increase)

;; full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; remove toolbar
(tool-bar-mode -1) 


(use-package which-key
  :ensure t
  :config (which-key-mode 1))


;;Brackets parentheses
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

(show-paren-mode 1)
(setq show-paren-style 'expression)
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;autocompletion
(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode)
  (setq ac-show-menu-immediately-on-auto-complete t))
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r b") 'helm-bookmarks))


;; this is for emacs to properly see environment variables
(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package dumb-jump :ensure t)

;;js
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  )
(use-package js2-refactor :ensure t)
(use-package tern-auto-complete :ensure t)
(use-package tern :ensure t
  :config
  (add-hook 'js2-mode-hook
	    (lambda ()
	        (tern-mode t)
		(general-define-key
		 :keymaps 'js2-mode-map
		 [remap evil-goto-definition] 'dumb-jump-go
		 [remap evil-jump-backward] 'dumb-jump-back)))
  (eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup))))
(use-package prettier-js :ensure t
  :config
  (defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))
    (add-hook 'web-mode-hook  'web-mode-init-prettier-hook))
 
;;web
(use-package web-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (general-define-key
		 :keymaps 'web-mode-map
		 [remap evil-goto-definition] 'dumb-jump-go
		 [remap evil-jump-backward] 'dumb-jump-back)
  (setq web-mode-markup-indent-offset 4))
  
    (add-hook 'web-mode-hook  'web-mode-init-hook))
(general-def '(insert normal visual) web-mode-map
  "C-c c" 'web-mode-element-close )

;; python
;; pip and virtualenv must be installed
;; create symbolic link for pip is a solution when there is only python3 installed

(use-package python :ensure t)
(use-package eval-in-repl :ensure t)
(use-package epc :ensure t)
(use-package jedi :ensure t
  :config
  (setq jedi:environment-virtualenv
      (append python-environment-virtualenv
              '("--python" "/usr/local/bin/python3"))))
(use-package elpy :ensure t)
(package-initialize)
(advice-add 'python-mode :before 'elpy-enable)
(add-hook 'python-mode-hook (lambda ()
			      (jedi:setup)
			      (general-define-key
			       :keymaps 'python-mode-map
			       [remap evil-goto-definition] 'jedi:goto-definition
			       [remap evil-jump-backward] 'dumb-jump-back)))

;; python specific keys
(tyrant-def 'normal python-mode-map
  "be" 'elpy-shell-send-buffer
  "efc" 'elpy-format-code
  "jd" 'jedi:show-doc)

(tyrant-def 'normal lisp-mode-map
  "be" 'slime-eval-buffer)

;; lisp !! NEED TO SET PATH TO SBCL!
(use-package slime :ensure t
  :config
  (setq inferior-lisp-program '"/usr/local/bin/sbcl"))


;;markdown
(use-package markdown-mode :ensure t)
(use-package flymd :ensure t
  :config
  (defun my-flymd-browser-function (url)
   (let ((browse-url-browser-function 'browse-url-firefox))
     (browse-url url)))
  (setq flymd-browser-open-function 'my-flymd-browser-function))



(use-package term+ :ensure t)


;;==========KEYS



(tyrant-def

    ""     nil
    "h"   (general-simulate-key "C-h")
    "u"   (general-simulate-key "C-u")
    "x"   (general-simulate-key "C-x")
    
    ;; Package manager
    "lp"  'list-packages

    ;; Quit operations
    "q"	  '(:ignore t :which-key "quit emacs")
    "qq"  'kill-emacs
    "qz"  'delete-frame

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffer")
    "bb"  'helm-buffers-list
    "bd"  'kill-this-buffer
    "<left>"  'next-buffer
    "<right>"  'previous-buffer
    "bq"  'kill-buffer-and-window
    "bR"  'rename-file-and-buffer
    "br"  'revert-buffer
    "be"  'eval-buffer

    ;; Describe
    "df"  'describe-function
    "dk"  'describe-key
    "dv"  'describe-variable
    "dp"  'describe-package
    "ds"  'describe-symbol
    "dm"  'describe-mode
    
    ;; Git
    "ga"  'magit-stage-modified
    "gc"  'magit-commit-create
    "gpu" 'magit-push-current-to-upstream
    "gpl" 'magit-pull-from-upstream
    "ggn" 'gist-buffer-private
    "ggl" 'gist-list
    
    ;; Window operations
    "w"   '(:ignore t :which-key "window")
    "wm"  'maximize-window
    "wh"  'split-window-vertically
    "wv"  'split-window-horizontally
    "wm"  'maximize-window
    "wu"  'winner-undo
    "TAB"  'other-window
    "wd"  'delete-window
    "wD"  'delete-other-windows

    ;; File operations
    "f"   '(:ignore t :which-key "files")
    "fc"  'write-file
    "fe"  '(:ignore t :which-key "emacs")
    "fed" (lambda () (interactive) (find-file "~/.emacs"))
    "feR" 'load-user-init-file
    "fj"  'dired-jump
    "fl"  'find-file-literally
    "fR"  'rename-file-and-buffer
    "fs"  'save-buffer

    ;;Project
    "pdd"  'projectile-discover-projects-in-directory
    "pp"   'projectile-switch-project
    "pm"   'projectile-command-map
    "pg"   'projectile-grep
    "pff"   'projectile-find-file
    "pfo"   'projectile-find-file-other-window
    "pa"   'projectile-add-known-project

    ;; Commands
    "cs"  'shell

    ;; Applications
    "a"   '(:ignore t :which-key "Applications")
    "ad"  'dired
    ":"   'shell-command
    ";"   'eval-expression
    "ac"  'calendar)


  (general-def 'normal doc-view-mode-map
    "j"   'doc-view-next-line-or-next-page
    "k"   'doc-view-previous-line-or-previous-page
    "gg"  'doc-view-first-page
    "G"   'doc-view-last-page
    "C-d" 'doc-view-scroll-up-or-next-page
    "C-f" 'doc-view-scroll-up-or-next-page
    "C-b" 'doc-view-scroll-down-or-previous-page)

  (general-def '(normal visual) outline-minor-mode-map
    "zn"  'outline-next-visible-heading
    "zp"  'outline-previous-visible-heading
    "zf"  'outline-forward-same-level
    "zB"  'outline-backward-same-level)

  (general-def 'normal package-menu-mode-map
    "i"   'package-menu-mark-install
    "U"   'package-menu-mark-upgrades
    "d"   'package-menu-mark-delete
    "u"   'package-menu-mark-unmark
    "x"   'package-menu-execute
    "q"   'quit-window)

  (general-def 'normal calendar-mode-map
    "h"   'calendar-backward-day
    "j"   'calendar-forward-week
    "k"   'calendar-backward-week
    "l"   'calendar-forward-day
    "0"   'calendar-beginning-of-week
    "^"   'calendar-beginning-of-week
    "$"   'calendar-end-of-week
    "["   'calendar-backward-year
    "]"   'calendar-forward-year
    "("   'calendar-beginning-of-month
    ")"   'calendar-end-of-month
    "SPC" 'scroll-other-window
    "S-SPC" 'scroll-other-window-down
    "<delete>" 'scroll-other-window-down
    "<"   'calendar-scroll-right
    ">"   'calendar-scroll-left
    "C-b" 'calendar-scroll-right-three-months
    "C-f" 'calendar-scroll-left-three-months
    "{"   'calendar-backward-month
    "}"   'calendar-forward-month
    "C-k" 'calendar-backward-month
    "C-j" 'calendar-forward-month
    "gk"  'calendar-backward-month
    "gj"  'calendar-forward-month
    "v"   'calendar-set-mark
    "."   'calendar-goto-today
    "q"   'calendar-exit)



(use-package suggest
  :general (tyrant-def "as" 'suggest))


;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  
  (define-key treemacs-mode-map (kbd "C-<return>") (lambda () (interactive) (treemacs-visit-node-no-split t)))
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

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
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ([f8]        . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)


;; END treemacs
;; docs: https://github.com/Alexander-Miller/treemacs


;; projetile
(use-package projectile :ensure t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p")  'projectile-command-map)
(add-to-list 'projectile-globally-ignored-directories "public") 
(add-to-list 'projectile-globally-ignored-directories "node_modules") 

;; mode-line
(use-package all-the-icons :ensure t)
;; once package installed need to run M-x all-the-icons-install-fonts

(use-package doom-modeline
  :ensure t
  :hook (after-init  . doom-modeline-mode))

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 3)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; Whether display icons in mode-line or not.
(setq doom-modeline-icon t)

;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display color icons for `major-mode'. It respects
;; `doom-modeline-icon' and `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display buffer modification icon. It respects `doom-modeline-icon'
;; and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display minor modes in mode-line or not.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Whether display buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display indentation information.
(setq doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display perspective name or not. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; Whether display icon for persp name. Nil to display a # sign. It respects `doom-modeline-icon'
(setq doom-modeline-persp-name-icon nil)

;; Whether display `lsp' state or not. Non-nil to display in mode-line.
(setq doom-modeline-lsp t)

;; Whether display github notifications or not. Requires `ghub` package.
(setq doom-modeline-github nil)

;; The interval of checking github.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display environment version or not
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python")
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
(setq doom-modeline-mu4e t)

;; Whether display irc notifications or not. Requires `circe' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)



;;My macros


;; Org mode
(setq org-todo-keywords
      '(
        (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
        ))

(setq org-todo-keyword-faces
      '(("IDEA" . (:foreground "GoldenRod" :weight bold))
        ("NEXT" . (:foreground "IndianRed1" :weight bold))   
        ("STARTED" . (:foreground "OrangeRed" :weight bold))
        ("WAITING" . (:foreground "coral" :weight bold)) 
        ("CANCELED" . (:foreground "LimeGreen" :weight bold))
        ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
        ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
        ))

(setq org-tag-persistent-alist 
      '((:startgroup . nil)
        ("HOME" . ?h) 
        ("RESEARCH" . ?r)
        ("TEACHING" . ?t)
        (:endgroup . nil)
        (:startgroup . nil)
        ("OS" . ?o) 
        ("DEV" . ?d)
        ("WWW" . ?w)
        (:endgroup . nil)
        (:startgroup . nil)
        ("EASY" . ?e)
        ("MEDIUM" . ?m)
        ("HARD" . ?a)
        (:endgroup . nil)
        ("URGENT" . ?u)
        ("KEY" . ?k)
        ("BONUS" . ?b)
        ("noexport" . ?x)  
        )
      )

(setq org-tag-faces
      '(
        ("HOME" . (:foreground "GoldenRod" :weight bold))
        ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
        ("TEACHING" . (:foreground "GoldenRod" :weight bold))
        ("OS" . (:foreground "IndianRed1" :weight bold))   
        ("DEV" . (:foreground "IndianRed1" :weight bold))   
        ("WWW" . (:foreground "IndianRed1" :weight bold))
        ("URGENT" . (:foreground "Red" :weight bold))  
        ("KEY" . (:foreground "Red" :weight bold))  
        ("EASY" . (:foreground "OrangeRed" :weight bold))  
        ("MEDIUM" . (:foreground "OrangeRed" :weight bold))  
        ("HARD" . (:foreground "OrangeRed" :weight bold))  
        ("BONUS" . (:foreground "GoldenRod" :weight bold))
        ("noexport" . (:foreground "LimeGreen" :weight bold))  
        )
)

(use-package org-bullets :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(tyrant-def 'normal org-mode-map
  "ot" 'org-todo
  "oil" 'org-insert-link
  "ool" 'org-open-at-point
  
  )


;;new line
(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
                             (and (looking-back ">") (looking-at "<"))
                             (and (looking-back "(") (looking-at ")"))
                             (and (looking-back "\\[") (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(general-def 'insert
  "RET" 'new-line-dwim)



;; gist management
