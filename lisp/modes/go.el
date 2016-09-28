(require 'go-mode)

(defun tc/local-tab-width ()
  (setq tab-width 2))

(setenv "GOPATH" "/home/tcrawley/w/go/current-workspace")

(add-hook 'go-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'go-mode-hook 'linum-mode)
(add-hook 'go-mode-hook 'tc/local-tab-width)

;; paredit
(add-hook 'go-mode-hook 'tc/turn-on-paredit-nonlisp)
(define-key go-mode-map "{" 'paredit-open-curly)
(define-key go-mode-map "}" 'paredit-close-curly-and-newline)
(define-key go-mode-map "\'" 'paredit-singlequote)


(add-hook 'before-save-hook 'gofmt-before-save)





