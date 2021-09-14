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

(fset 'yes-or-no-p 'y-or-n-p)
;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)


