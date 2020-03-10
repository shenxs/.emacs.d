;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;重新加载配置文件
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

;;重载配置文件
(global-set-key (kbd "<f3>") 'reload-init-file)
;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "M-n") 'new-frame)
(global-set-key (kbd "M-w") 'delete-frame)
(global-set-key (kbd "M-q") 'kill-emacs)
(global-set-key (kbd "M-c") 'evil-yank)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key (kbd "<f4>") 'delete-window)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(provide 'key-binding)
