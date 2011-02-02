(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(global-set-key (kbd "<M-return>") 'hippie-expand)
(global-set-key (kbd "<C-tab>") 'hippie-expand)


