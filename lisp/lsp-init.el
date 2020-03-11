
;;; Require
(require 'lsp-mode)
(require 'company-lsp)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Enable LSP backend.
(push 'company-lsp company-backends)

;; Configuration to fix LSP errors.
(setq lsp-enable-eldoc nil) ;we will got error "Wrong type argument: sequencep" from `eldoc-message' if `lsp-enable-eldoc' is non-nil
(setq lsp-message-project-root-warning t) ;avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
(setq create-lockfiles nil) ;we will got error "Error from the Language Server: FileNotFoundError" if `create-lockfiles' is non-nil

;; Python support for lsp-mode using pyls.
;; Install: pip install python-language-server
;;
;; When type os. in python file, pyls will crash.
;; So you need clone python-language-server and path https://github.com/palantir/python-language-server/issues/373
;; Then install patched version with command:
;;
;; sudo pip install .
;;
(add-hook 'python-mode-hook #'lsp-python-enable)

;; Ruby support for lsp-mode using the solargraph gem.
;; Install: gem install solargraph
;; NOTE: and you need put below line in your Gemfile, otherwise lsp-ruby will report tcp error.
;;
;; gem "solargraph"
;;
(add-hook 'ruby-mode-hook #'lsp-ruby-enable)

;; Javascript, Typescript and Flow support for lsp-mode
;;
;; Install:
;;
;; npm install -g typescript
;; npm install -g typescript-language-server
;;
;; Fixed error "[tsserver] /bin/sh: /usr/local/Cellar/node/10.5.0_1/bin/npm: No such file or directory" :
;; 
;; sudo ln -s /usr/local/bin/npm /usr/local/Cellar/node/10.5.0_1/bin/npm
;;
(add-hook 'js-mode-hook #'lsp-typescript-enable)
(add-hook 'typescript-mode-hook #'lsp-typescript-enable) ;; for typescript support
(add-hook 'js3-mode-hook #'lsp-typescript-enable) ;; for js3-mode support
(add-hook 'rjsx-mode #'lsp-typescript-enable) ;; for rjsx-mode support

(defun lsp-company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun lsp-js-hook nil
  (make-local-variable 'company-transformers)
  (push 'lsp-company-transformer company-transformers))

(add-hook 'js-mode-hook 'lsp-js-hook)


(provide 'lsp-init)
