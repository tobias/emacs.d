;; (add-hook 'after-init-hook 'half-blind)

(load-theme 'zenburn)
;; make the minibuffer prompt stand out
(set-face-attribute 'minibuffer-prompt nil
                    :foreground "black"
                    :background "yellow"
                    :weight 'bold)

(when (display-graphic-p)
  (if tc/macos-p
      (set-face-font `default "-apple-inconsolata-medium-r-normal--15-0-72-72-m-0-iso10646-1")
    (set-face-font `default "Inconsolata-10"))

  (defun fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

;; (defun prep-for-preso ()
;;   (interactive)
;;   (set-face-font `default "-apple-inconsolata-medium-r-normal--18-0-72-72-m-0-iso10646-1")
;;   (yas/global-mode t)
;;   (dired "~/presentations/rubyconfbr/rubyconfbr-demo/ideabox/"))








     
 


       
