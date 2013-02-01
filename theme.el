(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

(if tc/presentation-mode-p
    (load-theme 'half-blind)
  (progn
    (load-theme 'zenburn)
    ;; make active windows more obvious
    (set-face-attribute 'mode-line-inactive nil
                        :inherit 'mode-line
                        :background "#383838"
                        :foreground "#5f7f5f"
                        :box '(:line-width -1 :style pressed-button)
                        :weight 'light)))

;; make the minibuffer prompt stand out
(set-face-attribute 'minibuffer-prompt nil
                    :foreground "black"
                    :background "yellow"
                    :weight 'bold)

(when (display-graphic-p)
  (if tc/macos-p
      (set-face-font `default "-apple-inconsolata-medium-r-normal--15-0-72-72-m-0-iso10646-1")
    (set-face-font `default "Inconsolata-11"))

  (defun fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(when tc/presentation-mode-p
  (set-face-font `default "-apple-inconsolata-medium-r-normal--20-0-72-72-m-0-iso10646-1"))








     
 


       
