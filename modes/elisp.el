(add-hook 'emacs-lisp-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'emacs-lisp-mode-hook 'tc/run-lisp-coding-hooks)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)
