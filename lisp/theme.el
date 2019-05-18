(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

(defun tc/light-theme ()
  (interactive)
  (load-theme 'sorta-blind)
  (add-hook 'after-init-hook 'tc/light-customizations))

(defun tc/preso-theme ()
  (interactive)
  (load-theme 'half-blind)
  ;; if we don't force the foreground to nil, it will be
  ;; the gtk selection color. This lets font-lock decoration
  ;; remain active
  (set-face-foreground 'region nil))

(defun tc/light-customizations ()
  ;; make the cursor dark
  (set-face-attribute 'cursor nil
                      :background "grey65"))

(defun tc/dark-customizations ()
  ;; make the cursor whiteish
  (set-face-attribute 'cursor nil
                      :background "ghost white")
  ;; make the minibuffer prompt stand out
  (set-face-attribute 'minibuffer-prompt nil
                      :foreground "black"
                      :background "yellow"
                      :weight 'bold)
  ;; (set-face-attribute 'ido-subdir nil
  ;;                     :foreground "yellow")
  ;; make highlight easier on the eyes
  (set-face-attribute 'highlight nil
                      :background "gray25"
                      :foreground nil;;"gray90"
                      )
  ;; ditto for what I say in erc
  (set-face-attribute 'erc-input-face nil
                      :foreground "gold")
  ;; make irc notices fade into the night
  (set-face-attribute 'erc-notice-face nil
                      :foreground "SlateGray"
                      :height 0.8)
  ;; make mentions stand out
  (set-face-attribute 'erc-current-nick-face nil
                      :background "black"
                      :foreground "gold"
                      :weight 'bold)
  ;; code improvements
  (set-face-attribute 'font-lock-comment-face nil
                      :foreground "#73d216"
                      :slant 'italic)
  (set-face-attribute 'font-lock-string-face nil
                      :foreground "khaki")
  ;; powerline readability
  ;; (set-face-attribute 'powerline-active1 nil
  ;;                     :background "grey22"
  ;;                     :foreground "white")

  ;; if we don't force the foreground to nil, it will be
  ;; the gtk selection color. This lets font-lock decoration
  ;; remain active
  (set-face-foreground 'region nil)

  ;; adjust the colors used in the shell
  (setq ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "SteelBlue1" "#dc8cc3" "#93e0e3" "#dcdccc"])
  ;; (setq ansi-color-map (ansi-color-make-color-map))

  )

(defun tc/dark-theme ()
  (interactive)
  (load-theme 'tango-dark)
  (add-hook 'after-init-hook 'tc/dark-customizations))

(if tc/macos-p
    (setq tc/default-font "-apple-inconsolata-medium-r-normal--%s-0-72-72-m-0-iso10646-1")
  (setq tc/default-font "Liberation Mono-%s"))

(defvar embiggened-size 14)

(defun embiggen (size)
  (interactive (list (read-string (format "Font size (%s): " embiggened-size))))
  (when (>= (if (numberp size)
                size
              (string-to-number size))
            7)
    (setq embiggened-size size)
    (set-frame-font (format tc/default-font size))))

(when (display-graphic-p)
  (defun fullscreen ()
    "Toggle full screen"
    (interactive)
    (if (eq window-system 'x)
        (shell-command
         (concat "wmctrl -i -r "
                 (frame-parameter nil 'outer-window-id)
                 ;" -btoggle,fullscreen"
                 " -btoggle,maximized_vert,maximized_horz"))
      (set-frame-parameter
       nil 'fullscreen
       (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
        
  (if tc/presentation-mode-p
      (tc/preso-theme)
    (if tc/light-theme-p
        (tc/light-theme)
      (tc/dark-theme)))

  (if tc/presentation-mode-p
      (embiggen 16)
    (embiggen 14)))

;; flash the cursor line when jumping around
(beacon-mode 1)

(setq beacon-color "yellow"
      beacon-blink-when-focused t)

