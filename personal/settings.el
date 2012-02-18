(require 'cl)
(require 'saveplace)
(require 'uniquify)
(require 'ansi-color)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (global-hl-line-mode t))

(cua-mode nil)

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      xterm-mouse-mode t
      save-place-file (concat user-emacs-directory "places")
      column-number-mode t
      confirm-kill-emacs (quote y-or-n-p)
      show-paren-mode t
      blink-cursor-mode nil
      x-select-enable-clipboard t
      backup-directory-alist (list (cons "." (expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)

(load "echo-area-bell")
(load "php-mode")
(load "flymake-cursor")

;(global-linum-mode 1)

(global-auto-revert-mode t)

(delete-selection-mode t)

(mouse-avoidance-mode 'exile)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Split windows in Emacs 22 compatible way
(setq split-height-threshold nil)
(setq split-width-threshold most-positive-fixnum)

(put 'upcase-region 'disabled nil)

;; deft config
(require 'deft)
(setq deft-extension "md")
(setq deft-directory "~/Dropbox/notes/")
(setq deft-text-mode 'markdown-mode)

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
    (insert char)
    (if (< 0 arg) (forward-char -1)))

(server-start)


