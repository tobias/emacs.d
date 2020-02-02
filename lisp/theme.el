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
  )

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

  ;; other buffers
  (set-face-attribute 'auto-dim-other-buffers-face nil
                      :background "gray15")
  
  ;; header line
  (set-face-attribute 'header-line nil
                    :background "gray25"
                    :foreground "gray90"
                    :box '(:line-width -1 :color "*" :style released-button))

  ;; org mode improvements
  (set-face-attribute 'org-done nil
                      :foreground "DarkOliveGreen3"
                      :weight 'bold)
  (set-face-attribute 'org-tag nil
                      :foreground "yellow"
                      :weight 'bold)
  (set-face-attribute 'org-todo nil
                      :background "Black"
                      :foreground "Yellow"
                      :weight 'bold)
  
  ;; have ace-window char stand out more
  (set-face-attribute 'aw-leading-char-face nil
                      :background "yellow"
                      :foreground "black"
                      :weight 'bold)

  (set-face-attribute 'ag-hit-face nil
                      :foreground "CornflowerBlue"
                      :weight 'bold)

  (set-face-attribute 'compilation-info nil
                      :foreground "CornflowerBlue"
                      :weight 'bold)
 
 
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
    (setq tc/default-font "Jetbrains Mono-%s")
  (setq tc/default-font "JetBrainsMono-%s"))
(setq-default line-spacing 2)

(defvar embiggened-size (if tc/macos-p 13 12))

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
    (embiggen embiggened-size)))

;; flash the cursor line when jumping around
(beacon-mode 1)

(setq beacon-color "yellow"
      beacon-blink-when-focused t)

;; display ivy selection in overlay instead of minibuffer
;; https://github.com/tumashu/ivy-posframe
;; (require 'ivy-posframe)
;; (setq ivy-posframe-display-functions-alist
;;       '((swiper          . nil)
;;         (complete-symbol . ivy-posframe-display-at-point)
;;         (t               . ivy-posframe-display-at-frame-center))
      
;;       ivy-posframe-height-alist '((swiper . 20)
;;                                   (t      . 50))
;;       ivy-posframe-font (format tc/default-font (+ embiggened-size 2))
;;       ivy-posframe-parameters '((left-fringe  . 8)
;;                                 (right-fringe . 8)
;;                                 (background-color . "gray25")))
;; (ivy-posframe-mode 1)

;; mini-modeline - https://github.com/kiennq/emacs-mini-modeline
(require 'mini-modeline)

(defface tc/mini-modeline-msg-face
  '((t (:foreground "yellow")))
  "Face used for mini-modeline messages")
  
(defun tc/mini-modeline-msg ()
  "Customized mini modeline message that just applies the
tc/mini-modeline-msg-face face to it."
  (when mini-modeline--msg
    (propertize mini-modeline--msg
                'face 'tc/mini-modeline-msg-face)))

(setq mini-modeline-color "gray25"
      mini-modeline-l-format (quote (:eval (tc/mini-modeline-msg))))
(mini-modeline-mode 1)

;; auto-dim-other-buffers: https://github.com/mina86/auto-dim-other-buffers.el
(auto-dim-other-buffers-mode 1)
