(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

(defun tc/light-theme ()
  (load-theme 'sorta-blind))

(defun tc/dark-customizations ()
  ;; make the cursor whiteish
  (set-face-attribute 'cursor nil
                      :background "ghost white")
  ;; make the minibuffer prompt stand out
  (set-face-attribute 'minibuffer-prompt nil
                      :foreground "black"
                      :background "yellow"
                      :weight 'bold)
  (set-face-attribute 'ido-subdir nil
                      :foreground "yellow")
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
  (set-face-attribute 'powerline-active1 nil
                      :background "grey22"
                      :foreground "white")

  ;; if we don't force the foreground to nil, it will be
  ;; the gtk selection color. This lets font-lock decoration
  ;; remain active
  (set-face-foreground 'region nil)

  ;; adjust the colors used in the shell
  (setq ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "SteelBlue1" "#dc8cc3" "#93e0e3" "#dcdccc"])
  ;; (setq ansi-color-map (ansi-color-make-color-map))

  )

(defun tc/dark-theme ()
  (load-theme 'tango-dark)
  (add-hook 'after-init-hook 'tc/dark-customizations))

(setq tc/default-font "Liberation Mono")

(defvar embiggened-size)

(defun embiggen (size)
  (interactive (list (read-string (format "Font size (%s): " embiggened-size))))
  (when (>= (if (numberp size)
                size
              (string-to-number size))
            7)
    (setq embiggened-size size)
    (set-frame-font (format "%s-%s" tc/default-font size))
    (when (display-graphic-p)
      (fullscreen))))

(require 'powerline)
(powerline-default-theme)

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
      (progn
        ;;(tc/dark-theme)
        (load-theme 'half-blind)
        ;; if we don't force the foreground to nil, it will be
        ;; the gtk selection color. This lets font-lock decoration
        ;; remain active
        (set-face-foreground 'region nil))
    (if tc/light-theme-p
        (tc/light-theme)
      (tc/dark-theme)))

  (if tc/macos-p
      (set-face-font `default "-apple-inconsolata-medium-r-normal--16-0-72-72-m-0-iso10646-1")
    (if tc/presentation-mode-p
        (embiggen 14)
      (embiggen 9)))

  )
