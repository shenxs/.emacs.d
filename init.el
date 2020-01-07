(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像
;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
		      ;; --- Auto-completion ---
		      company
		      ;; --- Better Editor ---
		      hungry-delete
		      ;;swiper
		      ;;counsel
		      smartparens
		      ;; --- Major Mode ---
		      ;; --- Minor Mode ---
		      exec-path-from-shell
		      evil
		      evil-escape
		      ;; --- Themes ---
		      monokai-theme
		      one-themes
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

(server-start)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/evil")

(require 'package)
(require 'parenface)
(require 'evil)
(package-initialize)

(tool-bar-mode -1)
(menu-bar-mode -1)
(evil-mode 1)
(evil-escape-mode 1)
(xterm-mouse-mode 1)
(global-hl-line-mode 1)
(define-key evil-motion-state-map ";" 'evil-ex)
(setq-default evil-escape-key-sequence "jk")

(load-theme 'one-light t)

(add-hook 'after-init-hook 'global-company-mode)

;; 显示行号
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

;;重载配置文件
(global-set-key (kbd "<f3>") 'reload-init-file)    ; Reload .emacs file


;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(set-face-foreground 'paren-face "DimGary")
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(set-variable (quote scheme-program-name) "scheme")
(setq scheme-program-name "scheme")
(setq geiser-chez--binary "scheme")

(add-to-list 'auto-mode-alist '("\\.sc\\'" . scheme-mode))

;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;

(require 'cmuscheme)
(setq scheme-program-name "scheme")         ;; 如果用 Petite 就改成 "petite"

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
	       (get-buffer scheme-buffer)
	       (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
	       (mapcar (lambda (w) (buffer-name (window-buffer w)))
		       (window-list))
	       :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
	    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
