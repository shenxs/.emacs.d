(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

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
		      lsp-mode
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
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package racket-mode
  :ensure t
  :defer t
  )

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

(use-package snails
  :load-path (lambda () (expand-file-name "lisp/snails/" user-emacs-directory))
  :if (display-graphic-p)
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
  :init
  (use-package exec-path-from-shell :if (featurep 'cocoa) :defer t)
  :config
  ;; Functions for specific backends
  (defun snails-current-project ()
    (interactive)
    (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd)))
  (defun snails-active-recent-buffers ()
    (interactive)
    (snails '(snails-backend-buffer snails-backend-recentf)))
  (defun snails-everywhere ()
    (interactive)
    (snails '(snails-backend-everything snails-backend-mdfind)))
  :bind
  (
   ("M-s n" . snails)
   ("M-s g" . snails-current-project)
   ("M-s b" . snails-active-recent-buffers)
   ("M-s e" . snails-everywhere)))

(xterm-mouse-mode)
(global-hl-line-mode)
(electric-pair-mode)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(snails-content-buffer-face ((t (:background "#111" :height 110))))
 '(snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
 '(snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110)))))
