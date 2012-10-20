;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x m") 'shell)

(global-set-key (kbd "C-c n") 'cleanup-buffer)

(global-set-key (kbd "C-c s") 'swap-windows)

(global-set-key (kbd "C-c t") 'toggle-selective-display)

(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; navigation
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Map the window manipulation keys to meta 0, 1, 2, o
;; (global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
;; (global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
;; (global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument

;(global-set-key (kbd "M-1") '(lambda () (interactive) (switch-to-split-window 0)))
;(global-set-key (kbd "M-2") '(lambda () (interactive) (switch-to-split-window 1)))
;(global-set-key (kbd "M-3") '(lambda () (interactive) (switch-to-split-window 2)))
;(global-set-key (kbd "M-4") '(lambda () (interactive) (switch-to-split-window 3)))
;(global-set-key (kbd "M-5") '(lambda () (interactive) (switch-to-split-window 4)))

;(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
;(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap

;; To help Unlearn C-x 0, 1, 2, o
;; (global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
;; (global-unset-key (kbd "C-x 2")) ; was split-window-vertically
;; (global-unset-key (kbd "C-x 1")) ; was delete-other-windows
;; (global-unset-key (kbd "C-x 0")) ; was delete-window
;; (global-unset-key (kbd "C-x o")) ; was other-window

;; Never iconify...
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; force save buffer (useful for triggering autotest/watchr)
(global-set-key (kbd "C-c C-c t") '(lambda ()
                                    (interactive)
                                    (set-buffer-modified-p 1)
                                    (save-buffer)))

(global-set-key (kbd "C-x g") 'magit-status)
;(global-set-key (kbd "C-x C-g") 'magit-status)

(global-set-key (kbd "C-x C-d") 'ido-dired)

;; make the rinari navigation a bit shorter
;; (global-set-key (kbd "C-c f c") 'rinari-find-controller)
;; (global-set-key (kbd "C-c f e") 'rinari-find-environment)
;; (global-set-key (kbd "C-c f f") 'rinari-find-file-in-project)
;; (global-set-key (kbd "C-c f h") 'rinari-find-helper)
;; (global-set-key (kbd "C-c f i") 'rinari-find-migration)
;; (global-set-key (kbd "C-c f j") 'rinari-find-javascript)
;; (global-set-key (kbd "C-c f m") 'rinari-find-model)
;; (global-set-key (kbd "C-c f n") 'rinari-find-configuration)
;; (global-set-key (kbd "C-c f s") 'rinari-find-stylesheet)
;; (global-set-key (kbd "C-c f t") 'rinari-find-test)
;; (global-set-key (kbd "C-c f v") 'rinari-find-view)
;(global-set-key "\C-cfa" 'ack-in-project)

(global-set-key (kbd "C-c e") '(lambda ()
                                 (interactive)
                                 (dired (concat user-emacs-directory "personal"))))
                              
(global-set-key (kbd "C-c c") 'compile)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
