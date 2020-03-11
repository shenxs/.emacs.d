
;;; Code:

;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

(setenv "LANG" "zh_CN.UTF-8")

(show-paren-mode)
(auto-revert-mode)

;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

(provide 'better-default)
;;; better-default.el ends here
