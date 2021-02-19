;;; Code:

(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; 快速打开配置文件
(defun open-init-file()
  "Open init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;打开dashboard页面
(defun open-dashboard()
	(interactive)
  (switch-to-buffer "*dashboard*"))

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-mode)
(evil-escape-mode)


;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "<f4>") 'delete-window)
(global-set-key (kbd "<f12>") 'eval-buffer)
(global-set-key (kbd "M-n") 'new-frame)
(global-set-key (kbd "M-w") 'delete-frame)
(global-set-key (kbd "M-q") 'kill-emacs)
(global-set-key (kbd "s-c") 'evil-yank)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-b") 'lsp-goto-implementation)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(define-key evil-motion-state-map ";" 'evil-ex)
(setq-default evil-escape-key-sequence "jk")

(evil-leader/set-key
  "gs" 'magit-status
  "wd" 'delete-window
  "wD" 'delete-other-windows
  "hf" 'describe-function
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "hF" 'find-function-at-point
  "wc" 'whitespace-cleanup
  "w/" 'split-window-right
  "w-" 'split-window-below
  "//" 'evilnc-comment-or-uncomment-lines
  ";;" 'evilnc-comment-operator
  "qq" 'kill-emacs
  "qr" 'restart-emacs
  "ff" 'helm-find-files
  "bb" 'helm-buffers-list
  "bd" 'kill-buffer
  "'"  'toggle-ansi-term
  "bh" 'open-dashboard
  )

(global-set-key (kbd "M-\-") 'text-scale-decrease)
(global-set-key (kbd "M-=") 'text-scale-increase)

(add-hook 'lisp-interaction-mode
		  (lambda ()
			(define-key lisp-interaction-mode-map (kbd "<f5>") 'eval-buffer)))


(provide 'key-binding)

;;; key-binding.el ends here
