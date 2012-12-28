;; (add-hook 'after-init-hook 'half-blind)

(add-hook 'after-init-hook (lambda () (load-theme 'zenburn)))

(when window-system
  (if tc/macos-p
      (set-face-font `default "-apple-inconsolata-medium-r-normal--15-0-72-72-m-0-iso10646-1")
    (set-face-font `default "Inconsolata-10")))

;; (defun prep-for-preso ()
;;   (interactive)
;;   (set-face-font `default "-apple-inconsolata-medium-r-normal--18-0-72-72-m-0-iso10646-1")
;;   (yas/global-mode t)
;;   (dired "~/presentations/rubyconfbr/rubyconfbr-demo/ideabox/"))








     
 


       
