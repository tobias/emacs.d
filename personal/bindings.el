;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c n") 'cleanup-buffer)

(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; navigation
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Never iconify...
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; force save buffer (useful for triggering autotest/watchr)
(global-set-key (kbd "C-c C-c t") '(lambda ()
                                    (interactive)
                                    (set-buffer-modified-p 1)
                                    (save-buffer)))

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x C-d") 'ido-dired)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-x m") 'mu4e)
