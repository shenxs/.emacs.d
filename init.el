(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
		      use-package
		      ;; --- Auto-completion ---
		      ;; --- Better Editor ---
		      smartparens
		      multi-term
		      ;; --- Major Mode ---
		      paredit
		      ;; --- Minor Mode ---
		      exec-path-from-shell
		      evil
		      evil-escape
		      evil-leader
		      evil-magit
		      evil-nerd-commenter
		      ;; --- Themes ---
		      monokai-theme
		      one-themes
		      spacemacs-theme
		      ;;solarized-theme
		      ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(fset 'yes-or-no-p 'y-or-n-p)
(load "server")
(unless (server-running-p) (server-start))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/lisp/snails")

(require 'interface)
(require 'myscheme)
(require 'parenface)
(require 'evil)
(require 'snails)
(require 'evil-magit)
(require 'evil-leader)

(use-package helm
  :ensure t)

(use-package company
  :ensure t
  :config
  :init (global-company-mode 1))

(use-package magit
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package racket-mode)

(use-package lua-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode . rainbow-delimiters-mode)
	 (scheme-mode . rainbow-delimiters-mode)
	 (emacs-lisp-mode . rainbow-delimiters-mode)
	 (racket-mode . rainbow-delimiters-mode)))

(use-package hungry-delete
  :ensure t
  :hook ((racket-mode . hungry-delete-mode))
  :init
  (global-hungry-delete-mode))


(evil-mode 1)
(evil-escape-mode 1)
(xterm-mouse-mode 1)
(global-hl-line-mode 1)
(electric-pair-mode 1)
(setq-default evil-escape-key-sequence "jk")
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;重新加载配置文件
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

;;重载配置文件
(global-set-key (kbd "<f3>") 'reload-init-file)
(global-set-key (kbd "<spc>-;-;") 'comment-line)
;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "M-n") 'new-frame)
(global-set-key (kbd "M-w") 'delete-frame)
(global-set-key (kbd "M-q") 'kill-emacs)
(global-set-key (kbd "M-c") 'evil-yank)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key (kbd "<f4>") 'delete-window)

(add-hook 'snails-mode-hook
	  (lambda ()
	    (evil-emacs-state)))

(setq snails-default-backends
      '(snails-backend-buffer
	snails-backend-recentf
	snails-backend-current-buffer
	snails-backend-imenu
	snails-backend-mdfind
	snails-backend-bookmark
	snails-backend-rg))

(evil-leader/set-key
  "<SPC>" 'snails
  "gs" 'magit-status
  "wd" 'delete-window
  "hf" 'describe-function
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "hF" 'find-function-at-point
  "wc" 'whitespace-cleanup
  "//" 'evilnc-comment-or-uncomment-lines
  ";;" 'evilnc-comment-operator
  "qq" 'kill-emacs
  "ff" 'helm-find-files)

(define-key evil-motion-state-map ";" 'evil-ex)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (which-key company hungry-delete smartparens multi-term exec-path-from-shell evil evil-escape evil-leader magit monokai-theme one-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
