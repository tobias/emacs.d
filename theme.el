(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

(defun tc/light-theme ()
  (load-theme 'sorta-blind))

(defun tc/dark-customizations ()
  ;; make the minibuffer prompt stand out
  (set-face-attribute 'minibuffer-prompt nil
                      :foreground "black"
                      :background "yellow"
                      :weight 'bold)
  ;; make highlight easier on the eyes
  (set-face-attribute 'highlight nil
                      :background "gray40"
                      :foreground "gray90")
  ;; ditto for what I say in erc
  (set-face-attribute 'erc-input-face nil
                      :foreground "gold")
  ;; make irc notices fade into the night
  (set-face-attribute 'erc-notice-face nil
                      :foreground "SlateGray"
                      :height 0.8)
  ;; make mentions stand out
  (set-face-attribute 'erc-current-nick-face nil
                      :background "goldenrod4"
                      :foreground "gold"
                      :weight 'bold))
  
(defun tc/dark-theme ()
  (load-theme 'tango-dark)
  (add-hook 'after-init-hook 'tc/dark-customizations))

(when (display-graphic-p)
  (if tc/presentation-mode-p
      (load-theme 'half-blind)
    (if tc/light-theme-p
        (tc/light-theme)
      (tc/dark-theme)))
  
  (if tc/macos-p
      (set-face-font `default "-apple-inconsolata-medium-r-normal--16-0-72-72-m-0-iso10646-1")
    (if tc/presentation-mode-p
        (set-face-font `default "Liberation Mono-18")
      (set-face-font `default "Liberation Mono-10")))

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
