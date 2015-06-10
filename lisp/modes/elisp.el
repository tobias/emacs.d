(add-hook 'emacs-lisp-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'emacs-lisp-mode-hook 'tc/run-lisp-coding-hooks)
(add-hook 'emacs-lisp-mode-hook 'tc/remove-elc-on-save)

(defun tc/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)
