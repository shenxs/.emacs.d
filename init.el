;;; init.el --- my emacs config
;;; Commentary:

;;; code:
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)
(setq package-native-compile t)
(require 'package)
(require 'project)
(setq package-archives
      '(("org"       . "https://orgmode.org/elpa/")
	("gnu"       . "https://elpa.gnu.org/packages/")
	("melpa"     . "https://melpa.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "s-/") 'comment-line)

(setq make-backup-files nil
      create-lockfiles nil) ;; lock files will kill `npm start'

(use-package exec-path-from-shell
  :if (memq window-system '(ns mac))
  :ensure t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))
;; (add-to-list 'load-path "~/.emacs.d/elisp/scheme-complete/")
;; (add-to-list 'load-path "~/.emacs.d/elisp/scribble-mode/")
;; (require 'scheme-complete)
;; (require 'scribble-mode)
;; (add-hook 'scribble-mode-hook #'geiser-mode)
;; (autoload 'scheme-smart-complete "scheme-complete" nil t)
;; (eval-after-load 'scheme
  ;; '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))
;; (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
;; (add-hook 'scheme-mode-hook
  ;; (lambda ()
    ;; (make-local-variable 'eldoc-documentation-function)
    ;; (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
    ;; (eldoc-mode)))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package evil
  :ensure t
  :defer t
  :after evil-leader
  :init
  (setq evil-want-keybinding nil)
  :config
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ";") 'evil-ex)
    )
  (evil-mode 1)
  ;; (define-key evil-normal-state-map (kbd "<RET>") 'ivy-switch-buffer)
  )

(use-package evil-escape
  :ensure t
  :after evil
  :init (setq-default evil-escape-key-sequence "jk")
  :config (evil-escape-mode 1)
  )

(use-package evil-leader
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "gg" 'magit
    "ff" 'find-file
    "hf" 'describe-function
    "hv" 'describe-variable
    "ot" 'shell-pop
    "bb" 'ivy-switch-buffer
    "bd" 'kill-buffer
    "bh" 'dashboard-refresh-buffer
    "jj" 'avy-goto-char-2
    "jw" 'avy-goto-word-1
    "jl" 'avy-goto-line
    ))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-center-content t
	dashboard-items '((recents  . 10)
			  (bookmarks . 5)
			  (projects . 5)
			  (agenda . 5))
	dashboard-heading-icons '((recents   . "file-text")
				  (bookmarks . "bookmark")
				  (agenda    . "calendar")
				  (projects  . "briefcase")
				  (registers . "database")))
  :config
  (dashboard-setup-startup-hook)
  )

(use-package which-key
  :ensure t
  :defer t
  :hook (after-init . which-key-mode)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :hook (after-init . evil-collection-init)
  )

(use-package hungry-delete
  :ensure t
  :defer t
  :hook (after-init . global-hungry-delete-mode)
  )

(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package magit
  :defer t
  :ensure t)

(use-package git-gutter+
  :ensure t
  :defer t
  :config
  (progn
    (global-git-gutter+-mode)))

(use-package json-mode
  :defer t
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package racket-mode
  :defer t
  :ensure t)

(use-package geiser
  :ensure t
  :defer t
  )

(use-package  geiser-chez
  :ensure t
  :defer t
  )

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'" "\\.css\\'" "\\.html\\'")
  :commands web-mode)



(use-package lsp-mode
  :ensure t
  :defer t
  :init
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (web-mode . lsp)
	 (c++-mode . lsp)
	 )
  :commands lsp)


(use-package lsp-pyright
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))  ; or lsp-deferred



(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

(use-package prettier-js
  :defer t
  :ensure t)

(use-package ivy
  :ensure t
  :defer t
  :hook (after-init . ivy-mode))

(use-package lsp-ivy
  :ensure t
  :defer t
  :commands lsp-ivy-workspace-symbol)

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode))

(use-package lsp-treemacs
  :ensure t)

(use-package projectile
  :ensure t
  :defer t
  )

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode))

(use-package avy
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-'") 'avy-goto-char-2))

(use-package dap-mode
  :ensure t
  :defer t)

(use-package smartparens-config
  :ensure smartparens
  :defer t
  :init    (smartparens-global-mode t)
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  )

(use-package paredit
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'scheme-mode-hook 'paredit-mode)
    (add-hook 'racket-mode-hook 'paredit-mode)))

(use-package shell-pop
  :ensure t
  :defer t
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  )

(use-package treemacs
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "s-1") 'treemacs)
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

  )

(use-package treemacs-evil
  :ensure t
  :defer t)

(use-package treemacs-projectile
  :ensure t
  :defer t)

(use-package hydra
  :ensure t
  :defer t
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(paredit smartparens projectile lsp-treemacs yasnippet elisp-benchmarks racket-mode rainbow-delimiters json-mode which-key company-statistics company))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

