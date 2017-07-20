(require 'go-mode)
(require 'go-guru)

(defun tc/local-tab-width ()
  (setq tab-width 4))

(setenv "GOPATH" "/home/tcrawley/w/go/current-workspace")

(defun tc/go-mode-hook ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump);;'go-guru-definition)
  (tc/run-common-coding-hooks)
  (linum-mode)
  (tc/local-tab-width)
  (go-guru-hl-identifier-mode))

(add-hook 'go-mode-hook 'tc/go-mode-hook)


;; (defun my-go-mode-hook ()
;;   ; Use goimports instead of go-fmt
;;   (setq gofmt-command "goimports")
;;   ; Call Gofmt before saving
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   ; Customize compile command to run go build
;;   (if (not (string-match "go" compile-command))
;;       (set (make-local-variable 'compile-command)
;;            "go build -v && go test -v && go vet"))
;;   ; Godef jump key binding
;;   (local-set-key (kbd "M-.") 'godef-jump))
;; (add-hook 'go-mode-hook 'my-go-mode-hook)

;; (add-to-list 'load-path(concat (file-name-as-directory (getenv "GOPATH"))
;;                                "src/github.com/nsf/gocode/emacs"))
;; (require 'go-autocomplete)
;; (require 'auto-complete-config)
;; (ac-config-default)
