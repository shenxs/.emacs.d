
;;; Code:

;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

(setenv "LANG" "zh_CN.UTF-8")

(show-paren-mode)
;;display "lambda" as "λ"
(global-prettify-symbols-mode)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  )

(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

(display-battery-mode)
(display-time-mode)
(toggle-truncate-lines)

(provide 'better-default)
;;; better-default.el ends here
