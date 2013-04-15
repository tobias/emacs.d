(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))




(when (display-graphic-p)
  (if tc/presentation-mode-p
    (load-theme 'half-blind)
  (load-theme 'sorta-blind)
  ;; (progn
  ;;   (load-theme 'zenburn)
  ;;   ;; make active windows more obvious
  ;;   (set-face-attribute 'mode-line-inactive nil
  ;;                       :inherit 'mode-line
  ;;                       :background "#383838"
  ;;                       :foreground "#5f7f5f"
  ;;                       :box '(:line-width -1 :style pressed-button)
  ;;                       :weight 'light))
  )

  ;; make the minibuffer prompt stand out
  ;; (set-face-attribute 'minibuffer-prompt nil
  ;;                     :foreground "black"
  ;;                     :background "yellow"
  ;;                     :weight 'bold)

  (if tc/macos-p
      (set-face-font `default "-apple-inconsolata-medium-r-normal--16-0-72-72-m-0-iso10646-1")
    (set-face-font `default "Liberation Mono-10"))

  (defun fullscreen ()
    "Toggle full screen"
    (interactive)
    (if (eq window-system 'x)
        (shell-command
         (concat "wmctrl -i -r "
                 (frame-parameter nil 'outer-window-id)
                 " -btoggle,maximized_vert,maximized_horz"))
      (set-frame-parameter
       nil 'fullscreen
       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))))

(when tc/presentation-mode-p
  (set-face-font `default "-apple-inconsolata-medium-r-normal--20-0-72-72-m-0-iso10646-1"))








     
 


       
