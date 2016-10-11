(require 'go-mode)

(defun tc/local-tab-width ()
  (setq tab-width 4))

(setenv "GOPATH" "/home/tcrawley/w/go/current-workspace")

(add-hook 'go-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'go-mode-hook 'linum-mode)
(add-hook 'go-mode-hook 'tc/local-tab-width)
(add-hook 'go-mode-hook 'electric-pair-mode)

(add-hook 'before-save-hook 'gofmt-before-save)





