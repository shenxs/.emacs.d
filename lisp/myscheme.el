;;;;;;;;;;;;;;;;;;;;;;;
;; 我的scheme配置
;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(add-to-list 'auto-mode-alist '("\\.sc\\'" . scheme-mode))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(require 'cmuscheme)
(setq scheme-program-name "scheme")


;;绕过交互式启动限制 设置默认scheme实现
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary.
See variable `scheme-buffer'."
  (unless (and scheme-buffer
	       (get-buffer scheme-buffer)
	       (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process.  See variable `scheme-buffer'")))

;;发送整个buffer到scheme
(defun scheme-send-buffer ()
  "Send the current buffer to the inferior Scheme process."
  (interactive)
  (save-excursion
   (end-of-buffer)
   (let ((end (point)))
     (beginning-of-buffer)
     (scheme-send-region (point) end))))

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

(defun scheme-send-buffer-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-buffer))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-buffer-split-window)
	    (define-key scheme-mode-map (kbd "<f7>") 'scheme-send-last-sexp-split-window)
	    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))

(provide 'myscheme)
;;; myscheme.el ends here
