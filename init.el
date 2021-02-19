;;; init.el --- Richard's configuration

;;; Commentary:
;; This is my personal Emacs configuration.  Nothing more, nothing less.

;;; Code:
(load "server")
(unless (server-running-p) (server-start))


;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;;netword proxy setting
;; (setq url-gateway-method 'socks)
;; (setq socks-server '("Default server" "127.0.0.1" 1080 5))


;;set mirror
;; (setq package-archives '(("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")
;; 			 ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
;; 			 ))

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'interface)
(require 'key-binding)
(require 'better-default)

(require 'myscheme)
(require 'lsp-init)

(use-package monokai-theme
  :defer t)
(use-package one-themes
  :defer t)
(use-package doom-themes
  :defer t)
(use-package solarized-theme
  :defer t)
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
	(setenv "SHELL" "/bin/zsh")
	(exec-path-from-shell-initialize)
	(exec-path-from-shell-copy-envs '("PATH"))))

(use-package evil
  :defer t
  :init
  (use-package evil-escape)
  (use-package evil-leader)
  (use-package evil-magit)
  :config
  (evil-mode)
  (evil-escape-mode))

(use-package beancount
  :load-path "~/.emacs.d/plugins/beancount-mode"
  :ensure nil
  :mode ("\\.bean$" . beancount-mode)
  :defer t)

(use-package restart-emacs
  :defer t)
(use-package paredit
  :defer t)
(use-package smartparens
  :defer t)
(use-package multi-term
  :defer t)

(use-package imenu-list
  :defer t
  :config
  (setq imenu-list-focus-after-activation t))

(use-package helm
  :defer t)

(use-package company
  :defer t
  :init
  (setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))
  :config
  (use-package company-lsp)
  :init (global-company-mode 1))

(use-package magit
  :defer t
  )

(use-package flycheck
  :defer t
  )

(use-package which-key
  :init (which-key-mode))

(use-package racket-mode
  :defer t
  )
(use-package quickrun
  :defer t)

(use-package rust-mode
  :defer t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (bookmarks . 5)
                        (agenda . 5)
                        (registers . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  
  (defun dashboard-goto-recent-files ()
	"Go to recent files."
	(interactive)
	(funcall (local-key-binding "r")))

  (defun dashboard-goto-projects ()
	"Go to projects."
	(interactive)
	(funcall (local-key-binding "p")))
  
  (with-eval-after-load 'evil
	(evil-define-key 'normal dashboard-mode-map
	  "g" 'dashboard-refresh-buffer
	  "p" 'dashboard-goto-projects
	  "r" 'dashboard-goto-recent-files
	  "R" 'restore-session))
  )

(use-package lsp-mode
  :ensure t
  :init
  (add-to-list 'company-backends 'company-capf)
  :config
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'java-mode-hook #'lsp)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  :defer t
  :hook (
		 (lsp-mode . lsp-enable-which-key-integration)
		 )
  :config (setq lsp-completion-enable-addtional-text-edit nil)
  )
(setq company-minimum-prefix-length 1
	  company-idle-delay 0.0)

(use-package ccls
  :defer t
  :hook
  ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp))))

(setq ccls-executable "/usr/local/bin/ccls")

(use-package company-lsp
  :ensure t
  :defer t
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

(use-package yasnippet :config (yas-global-mode))
(use-package hydra
  :defer t)
(use-package lsp-ui
  :defer t
  :config
  (setq lsp-ui-doc-delay 5.0
		lsp-ui-sideline-enable nil
		lsp-ui-sideline-show-symbol nil)
  )

(use-package treemacs
  :defer t
  :init
  (add-hook 'treemacs-mode-hook
			(lambda () (treemacs-resize-icons 15))))

;; to enable the lenses

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
   '("0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(dashboard yasnippet projectile lsp-java which-key company hungry-delete smartparens multi-term exec-path-from-shell evil evil-escape evil-leader magit monokai-theme one-themes))
 '(tab-width 4))
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
