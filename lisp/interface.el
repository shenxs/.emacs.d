
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
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;;设置主题
(load-theme 'spacemacs-dark t)
(provide 'interface)

