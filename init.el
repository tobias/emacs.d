(package-initialize)

(defconst tc/macos-p
  (eq system-type 'darwin)
  "Are we on MacOS?")

(defconst tc/presentation-mode-p
  (getenv "EMACS_PRESENTATION_MODE"))

(defconst tc/presentation-name
  (getenv "EMACS_PRESENTATION_NAME"))

(defconst tc/light-theme-p
  (getenv "EMACS_LIGHT_THEME"))

;; we need to load erc before custom.el and theme.el since they refer to
;; erc faces
(use-package erc)

;; keep customize settings in their own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; setup and use the same path as fish
(let ((path (split-string (shell-command-to-string
                           (format "%s/fish -c 'echo -n $PATH'"
                                   (if tc/macos-p
                                       "/usr/local/bin"
                                     "/usr/bin")))
                          " ")))
  (setenv "PATH" (mapconcat 'identity path ":"))
  (setq exec-path (append path exec-path)))

;; but use bash for executing commands, since fish doesn't like the
;; syntax emacs gives `find`
(setq shell-file-name "/bin/bash")

(setq lisp-dir (concat user-emacs-directory "lisp/"))

;; install any needed packages
(load (concat lisp-dir "packages"))

;; once packages are loaded and on the load-path, prepend .emacs.d/ and lib/
(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path (concat user-emacs-directory "lib"))

;; en/decrypt .gpg files automatically
(use-package epa-file)

;; get rid of ui cruft
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(when (display-graphic-p)
  ;; configure settings for use under a wm
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (scroll-bar-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode t)
  (setq-default cursor-type  '(bar . 2))
  (global-hl-line-mode t)
  (mouse-avoidance-mode 'exile))

;; Split windows in Emacs 22 compatible way
(setq split-height-threshold nil
      split-width-threshold  most-positive-fixnum)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; use the modeline to indicate the bell instead of sound or a big
;; black block in the middle of the screen (wtf wants that?)
(when (not tc/presentation-mode-p)
  (setq visible-bell t)
  (load "echo-area-bell"))

;; always show the column number in the mode line
(column-number-mode t)

;; save the bookmarks file every time I add one
(setq bookmark-save-flag 0)

;; prefer utf8
(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; start with an empty scratch buffer
(setq initial-scratch-message nil
      inhibit-startup-message t)

;; ask if I want to quit
(setq confirm-kill-emacs 'y-or-n-p)

;; store backup files in one dir instead of littering the fs
;; (setq backup-directory-alist
;;       (list (cons "." (expand-file-name "backups" user-emacs-directory))))

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; don't create lock files (emacs 24.3 and up)
(setq create-lockfiles nil)

;; save my place in visited files
(use-package saveplace
  :init
  (setq save-place-file (concat user-emacs-directory "places"))
  (set-default 'save-place t))

;; do a better job of making buffer names unique
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

;; Allow y for yes - I type enough as it is
(defalias 'yes-or-no-p 'y-or-n-p)

;; please don't insert tabs
(set-default 'indent-tabs-mode nil)

;; clean up whitespace before saving
;;(add-hook 'before-save-hook 'whitespace-cleanup)
;;(setq before-save-hook nil)

;; auto-revert any open buffers if they change on disk, and do the
;; same for dired. In both cases, don't tell me every time
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose                 nil)

;; repeat held down keys afer a tenth of a second delay
(setq echo-keystrokes 0.1)

;; show as much decoration as possible
(setq font-lock-maximum-decoration t)

;; allow typing to replace the selected region
(delete-selection-mode t)

;; kill entire line (including \n)
(setq kill-whole-line t)

;; always show empty lines at end of buffer
(set-default 'indicate-empty-lines t)

;; setup completion
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

;; mac specific options
(when tc/macos-p
  (setq mac-option-key-is-meta t
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'meta)
        
  (setq-default ispell-program-name "aspell")

  ;; don't open a new frame when the os tells emacs to open a file
  (setq ns-pop-up-frames nil))

;; speed up tramp
(setq tramp-default-method "ssh")

;; setup ido
;; (ido-mode t)
;; (setq ido-enable-flex-matching          t
;;       ido-enable-last-directory-history nil)
;; (require 'ido-completing-read+)
;; (ido-ubiquitous-mode t)

;; make ido list files vertically
;; (require 'ido-vertical-mode)
;; (ido-vertical-mode)

;; disable upcase-region because I often fat-finger it. I could also
;; unbind C-x C-u I supppose.
(put 'upcase-region 'disabled nil)

;; Fixes "ls does not support --dired; see `dired-use-ls-dired' for
;; more details." on MacOS w/Homebrew
;; install with: brew install coreutils
(let ((gls "/usr/local/bin/gls"))
  (when (file-exists-p gls)
    (setq insert-directory-program gls)))

;; advise zap-to-char to delete *up to* char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
    (insert char)
    (if (< 0 arg) (forward-char -1)))

;; use a custom wrapper around zsh
(setenv "ESHELL" (expand-file-name "~/bin/eshell"))

;; Never background/iconify
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; I often get the buffer list by accident
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)

;; revert - this shadows ido-find-file-read-only, but why would I want
;; to edit a file as read-only?
(global-set-key (kbd "C-x C-r") 'revert-buffer)

(when (not tc/presentation-mode-p)
  ;; use ace-window to jump between windows
  (use-package ace-window
    ;; force loading so we can set face attributes in theme.el
    :demand t
    :init
    (setq aw-keys
          '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    :bind
    (("C-:" . ace-window)
     ("C-x :" . ace-window))))

;;(global-set-key (kbd "C-x C-d") 'ido-dired)

;; alter the font size for the current buffer
(define-key global-map (kbd "C-x C-+") 'text-scale-increase)
(define-key global-map (kbd "C-x C--") 'text-scale-decrease)

;; lookup the word at point in the dict
(global-set-key (kbd "C-c s") 'ispell-word)

;; make renaming buffers easier
(global-set-key (kbd "C-c r") 'rename-buffer)

;; don't let the current window be used for a different buffer
(global-set-key (kbd "C-x p") 'tc/toggle-current-window-dedication)

;; jump to the begining of the text on the line. a second C-a jumps to
;; the beginning
(global-set-key (kbd "C-a") 'tc/smarter-move-beginning-of-line)

;; remember window configurations - walk through them with C-c left,
;; C-c right
(winner-mode 1)

;; always pair, electrically
(electric-pair-mode)

;; browse-kill-ring
(use-package browse-kill-ring
  :init
  (browse-kill-ring-default-keybindings))

;; add the system clipboard to the kill ring
(setq save-interprogram-paste-before-kill t)

(use-package ivy
  :config
  (ivy-mode))

(use-package swiper
  :bind
  (("C-s" . swiper)
   :map swiper-map
   ("C-." .
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
   ("M-." .
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word)))))))
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; (global-set-key (kbd "M-x") 'counsel-M-x)

;; amx - a better M-x
(use-package amx
  :bind
  ("M-x" . amx)
  ("M-X" . amx-major-mode-commands))

;; extra help fns - brought in for describe-keymap, mainly
(use-package help-fns+)

;; semantic region expansion
(use-package expand-region
  :bind
  ("M-2" . er/expand-region))

;; indent after return
(define-key global-map (kbd "RET") 'newline-and-indent)

;; disable magit's taking over of M-w
(use-package magit
  :bind (:map magit-mode-map
              ("M-w" . nil)))

;; use C-. for pop-tag-mark, it's easier than M-*
(global-set-key (kbd "C-.") 'pop-tag-mark)

;; display git status for the line in the gutter in all git-controlled files
(global-git-gutter-mode +1)

(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;; display matching indents
(use-package indent-guide
  :config
  (indent-guide-global-mode))

;; override the keybinding for command-log-mode, since it overlaps
;; with my weekpage bindings
(global-unset-key (kbd "C-c o"))
;; (global-set-key (kbd "C-c l") 'clm/toggle-command-log-buffer)

;; use zoom-mode to auto-adjust window sizes
(use-package zoom
  ;; the below binding forces deferment, but that binding is rarely
  ;; used, so we force loading of zoom since we want it active all the
  ;; time
  :demand t
  ;; use zoom to rebalance windows instead of the default algo
  :bind ("C-x +" . zoom)
  :config (zoom))

;; load everything else
(load "clubhouse-shared")
(load "header")
(load "ffip")
(load "functions")

;;(load "ftf")
;;(load "auto-complete-init")
(load "coding-utils")
;;(load "modes/adoc")
(load "modes/clojure")
(load "modes/dired")
(load "modes/elisp")
(load "modes/fennel")
(load "modes/git-commit")
;;(load "modes/go")
(load "modes/inferior-lisp")
;;(load "modes/java")
(load "modes/org")
;;(load "modes/python")
;;(load "modes/ruby")
(load "modes/javascript")
(load "modes/magit")
(load "modes/restclient")
(load "modes/sh")
(load "modes/shell")
;;(load "modes/term")
(load "modes/text")
(load "modes/markdown")

(when (not tc/presentation-mode-p)
  (load "modes/irc")
  ;; (when (require 'mu4e nil :noerror)
  ;;   (load "modes/mail"))
  )

(load "theme")

(when tc/presentation-mode-p
  (load "presentation")
  (when tc/presentation-name
    (load (concat user-emacs-directory "presentations/" tc/presentation-name))))

;; start an emacs server
(use-package server
  :config
  (unless (server-running-p)
    (add-hook 'after-init-hook 'server-start)))

(add-hook 'after-init-hook 'tc/startup-buffers)

