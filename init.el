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

;;interface
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "s-/") 'comment-line)

(setq make-backup-files nil
      create-lockfiles nil) ;; lock files will kill `npm start'

(use-package exec-path-from-shell
  :if (memq window-system '(ns mac))
  :ensure t
  :config (exec-path-from-shell-initialize))

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
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ";") 'evil-ex)
    )
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :after evil
  :init (setq-default evil-escape-key-sequence "jk")
  :config (evil-escape-mode 1)
  )

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
  :defer 5
  :hook (after-init . which-key-mode)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :defer 5
  :hook (after-init . evil-collection-init)
  )

(use-package hungry-delete
  :ensure t
  :hook (after-init . global-hungry-delete-mode)
  )

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package magit
  :defer 3
  :ensure t)

(use-package git-gutter+
  :ensure t
  :config
  (progn
    (global-git-gutter+-mode)))

(use-package json-mode
  :defer 4
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package racket-mode
  :defer 2
  :ensure t)

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode ("\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'" "\\.css\\'" "\\.html\\'")
  :commands web-mode)



(use-package lsp-mode
  :ensure t
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq   lsp-keep-workspace-alive nil
          lsp-signature-auto-activate nil
          lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-workspace-status-enable nil

          lsp-enable-file-watchers nil
          lsp-enable-folding nil
          lsp-enable-symbol-highlighting nil
          lsp-enable-text-document-color nil

          lsp-enable-indentation nil
          lsp-enable-on-type-formatting nil)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (web-mode . lsp)
	 (c++-mode . lsp)
	 )
  :commands lsp)


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred



(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package prettier-js
  :ensure t)

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package lsp-treemacs
  :ensure t)

(use-package projectile
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-'") 'avy-goto-char-2))

(use-package dap-mode
  :ensure t)

(use-package smartparens-config
  :ensure smartparens
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
  :bind (("C-`" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
   ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(paredit smartparens projectile lsp-treemacs yasnippet elisp-benchmarks helm racket-mode rainbow-delimiters json-mode which-key company-statistics company))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

