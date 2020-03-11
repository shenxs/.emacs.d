
;;; Code:
;;设置字体
(set-frame-font "Hack 13" nil t)
;;关闭工具栏
(tool-bar-mode -1)
;;关闭菜单栏
(menu-bar-mode -1)

;;图形模式下关闭滚动条
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      ))

;; 显示行号
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;;设置主题
;; (load-theme 'spacemacs-dark t)

(require 'doom-themes)

;;; Settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t  ; if nil, italics is universally disabled

      ;; doom-one specific settings
      doom-one-brighter-modeline nil
      doom-one-brighter-comments nil)

;; Load the theme (doom-one, doom-dark, etc.)
(load-theme 'doom-one t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode))


;;; OPTIONAL
;; brighter source buffers (that represent files)
;; (add-hook 'find-file-hook #'doom-buffer-mode-maybe)
;; ...if you use auto-revert-mode
;; (add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
;; And you can brighten other buffers (unconditionally) with:
;; (add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)

;; brighter minibuffer when active
()
;; (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Enable nlinum line highlighting
;; (doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode

(provide 'interface)
;;; interface.el ends here

