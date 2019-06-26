;; load emacs 24's package system. Add MELPA repository.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (>= emacs-major-version 24)

(require 'package)
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(server-start)

;; Installing packages if they don't exist
(defvar myPackages
  '(better-defaults
    elpy
    material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)
)

  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-cwd-line-style (quote text))
 '(neo-fit-to-contents t)
 '(neo-smart-open t)
 '(neo-theme (quote classic))
 '(neo-vc-integration (quote (face)))
 '(package-selected-packages
   (quote
    (use-package markdown-mode auctex speed-type multiple-cursors wanderlust gist elpy diff-hl neotree projectile eproject latex-preview-pane powerline alpha monokai-theme auto-complete smex nlinum-relative material-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(load-theme 'wombat)
(load-theme 'monokai t)

(setq frame-title-format "Emacs")

(menu-bar-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(autopair-global-mode)

(global-set-key (kbd "M-x") 'smex)

(global-undo-tree-mode)

(global-set-key (kbd "M-/") 'undo-tree-visualize)

(global-set-key (kbd "C-M-z") 'switch-window)

(global-set-key (kbd "C->") 'ace-jump-mode)

(require 'alpha)


(global-set-key (kbd "C-M-)") 'transparency-increase)
(global-set-key (kbd "C-M-(") 'transparency-decrease)

(powerline-center-theme)

(setq powerline-default-separtor 'wave)

;; LaTex preview
(latex-preview-pane-enable)


(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


(add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
		(define-key evil-normal-state-local-map (kbd "-") #'neotree-enter-horizontal-split)
		(define-key evil-normal-state-local-map (kbd "|") #'neotree-enter-vertical-split)))



;;(defun neotree-project-dir ()
 ;;   "Open NeoTree using the git root."
 ;;   (interactive)
 ;;   (let ((project-dir (projectile-project-root))
 ;;         (file-name (buffer-file-name)))
 ;;     (neotree-toggle)
 ;;     (if project-dir
 ;;         (if (neo-global--window-exists-p)
 ;;             (progn
 ;;               (neotree-dir project-dir)
 ;;               (neotree-find file-name)))
 ;;       (message "Could not find git project root."))))


(global-diff-hl-mode)

(elpy-enable)


(require 'python)
(defun python--add-debug-highlight ()
  "Adds a highlighter for use by `python--pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))
(define-key python-mode-map (kbd "<f5>") 'python-insert-breakpoint)

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p major-mode 'python-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward (concat "^\\s-*" python--pdb-breakpoint-string "$")
                               (point-max) t)
            ;; set COMINT argument to `t'.
            (ad-set-arg 1 t))))))




(defalias 'yes-or-no-p 'y-or-n-p) ;; always use y or n never print yes or no



;;(with-eval-after-load 'evil
 ;; ;; use evil mode in the buffer created from calling `list-packages'.
 ;; (add-to-list 'evil-buffer-regexps '("*Packages*" . normal))
;;
 ;; (with-eval-after-load 'package
 ;;   ;; movement keys j,k,l,h set up for free by defaulting to normal mode.
 ;;   ;; mark, unmark, install
 ;;   (evil-define-key 'normal package-menu-mode-map (kbd "m") #'package-menu-mark-install)
 ;;   (evil-define-key 'normal package-menu-mode-map (kbd "u") #'package-menu-mark-unmark)
 ;;   (evil-define-key 'normal package-menu-mode-map (kbd "x") #'package-menu-execute)))


;; Multiple cursors
(require 'multiple-cursors)


(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;; LaTeX
;;; This file is part of the Emacs Dev Kit

;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)

;; use evince for dvi and pdf viewer
;; evince-dvi backend should be installed
;;(setq TeX-view-program-selection
;;      '((output-dvi "DVI Viewer")
;;        (output-pdf "PDF Viewer")
;;        (output-html "Google Chrome")))
;;(setq TeX-view-program-list
;;      '(("DVI Viewer" "evince %o")
;;        ("PDF Viewer" "evince %o")
;;        ("Google Chrome" "google-chrome %o")))
;;
;;(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;;(add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))

;;(provide 'auctex-config)

;; latex (autex)	
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;;Eliminates the necessity for the save command before compilation is completed
(setq TeX-save-query nil)
	  

;; Setting path env variable	  
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))  
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))  
(setq exec-path (append exec-path '("/usr/local/bin")))

(defun my-clear-message ()
  (interactive)
  (message nil))

(global-set-key (kbd "C-c c") 'my-clear-message)



;; Function and binding for creating a new scratch buffer
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))



(global-set-key (kbd "C-c C-n") 'create-scratch-buffer)

;; function to open config file
(defun my-edit-configuration ()
  "Open init file."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-x C-8") 'my-edit-configuration)
