;;; init.el --- Richard's configuration

;;; Commentary:
;; This is my personal Emacs configuration.  Nothing more, nothing less.

;;; Code:
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(load "server")
(unless (server-running-p) (server-start))

;; cl - Common Lisp Extension
(require 'cl-lib)

;; Add Packages
(defvar my/packages '(
		      use-package
		      ;; --- Better Editor ---
		      smartparens
		      multi-term
        	      company-lsp
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
		      doom-themes
		      one-themes
		      spacemacs-theme
		      ;;solarized-theme
		      ) "Default packages.")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  "Check if all the package is installed."
  (cl-loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (cl-return nil)
	finally (cl-return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'interface)
(require 'key-binding)
(require 'evil)
(require 'evil-magit)
(require 'evil-leader)
(require 'better-default)
(require 'myscheme)
(require 'lsp-init)

(use-package benchmark-init
  :ensure t
  :init
  (benchmark-init/activate)
  :hook
  (after-init . benchmark-init/deactivate))

(use-package helm
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :config
  :init (global-company-mode 1))

(use-package magit
  :defer t
  :ensure t)

(use-package flycheck
  :ensure t
  )

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package racket-mode
  :ensure t
  :defer t
  )
(use-package quickrun
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package lsp-mode
  :ensure t
  :defer t
  :hook (
	 (python-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (c++-mode .lsp-deferred)
	 (rust-mode . lsp-deferred)
	 ))
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package company-lsp
  :ensure t
  :commands company-lsp)
;; if you are helm user
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lua-mode
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode . rainbow-delimiters-mode)
	 (scheme-mode . rainbow-delimiters-mode)
	 (emacs-lisp-mode . rainbow-delimiters-mode)
	 (racket-mode . rainbow-delimiters-mode)))

(use-package hungry-delete
  :ensure t
  :hook (racket-mode . hungry-delete-mode)
  :init
  (global-hungry-delete-mode 1))

(xterm-mouse-mode)
(global-hl-line-mode)
(electric-pair-mode)


;; TODO 退出term时依然会有问题，偶尔窗口不会关闭，待优化，不影响使用
(defun oleh-term-exec-hook ()
  "Term terinate hook.
Kill buffer and close Window after the term exit."
  (let* ((buff (current-buffer))
	 (win  (selected-window))
	 (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
	(if (string= event "finished\n")
	    (and (kill-buffer ,buff)
		 (delete-window ,win)))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

(defun toggle-ansi-term ()
  "Toggle ansi term bottom."
  (interactive)
  (if (string-prefix-p "*ansi-term*" (buffer-name (current-buffer)) )
      (delete-window)
    (let ((buffer-name "*ansi-term*"))
      (cond
       ((not (cl-find buffer-name
		   (mapcar (lambda (b) (buffer-name b)) (buffer-list))
		   :test 'equal))
	(split-window-vertically (floor (* 0.68 (window-height))))
	(other-window 1)
	(ansi-term "zsh"))
       ((not (cl-find buffer-name
		   (mapcar (lambda (w) (buffer-name (window-buffer w)))
			   (window-list))
		   :test 'equal))
	(split-window-vertically (floor (* 0.68 (window-height))))
	(other-window 1)
	(switch-to-buffer buffer-name))
       (t (switch-to-buffer-other-window buffer-name))))))


(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )


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
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
