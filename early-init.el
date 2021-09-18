;;; early-init.el

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)


(setq read-process-output-max (* 1024 1024)) ;; 1mb



;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)
(setq inhibit-startup-message nil)


;;interface
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)
(global-hl-line-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)



;;basic setup
(setq ring-bell-function 'ignore)


;; Set language environment to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;更好的换行展示
(global-visual-line-mode 1)

;;自动括号和定位
(electric-pair-mode 1)

;;设置tab宽度
(setq-default tab-width 4)

;;高亮括号
(show-paren-mode 1)


;;展示行尾空格

(setq show-trailing-whitespace 1)

;;防止文件过长
(global-so-long-mode 1)
