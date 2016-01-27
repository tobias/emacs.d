(require 'flymake-cursor)
(require 'linum)
(require 'diminish)
(require 'rainbow-delimiters)

(defun tc/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (diminish 'auto-fill-function))

(defun tc/turn-on-whitespace ()
  (whitespace-mode t))

(defun tc/turn-on-paredit ()
  (paredit-mode t)
  (diminish 'paredit-mode)
  (when (not (display-graphic-p))
    (define-key paredit-mode-map (kbd "M-[ c") 'paredit-forward-slurp-sexp)
    (define-key paredit-mode-map (kbd "M-[ d") 'paredit-forward-barf-sexp)))

(defun tc/turn-on-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (tc/turn-on-paredit))

(defun tc/turn-on-flycheck ()
  (require 'flycheck)
  (flycheck-mode t))

(defun tc/turn-on-show-paren ()
  (show-paren-mode t))

(defun tc/turn-on-folding ()
  (hs-minor-mode t)
  (fold-dwim-org/minor-mode t))

(defun tc/turn-on-idle-highlight ()
  (idle-highlight t)
  ;; this will complain since hi-lock-mode is only active during an
  ;; idle highlight, so we ignore any errors
  (ignore-errors
    (diminish 'hi-lock-mode)))

(defun tc/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun tc/enable-dwim-fold ()
  (interactive)
  (hs-minor-mode)
  (local-set-key (kbd "C-c TAB") 'fold-dwim-org/minor-mode))

(add-hook 'tc/common-coding-hooks 'tc/local-comment-auto-fill)
(add-hook 'tc/common-coding-hooks 'tc/add-watchwords)
(add-hook 'tc/common-coding-hooks 'tc/enable-dwim-fold)

(when (not tc/presentation-mode-p)
  (add-hook 'tc/common-coding-hooks 'linum-on)
  (add-hook 'tc/common-coding-hooks 'tc/turn-on-idle-highlight)
  (require 'pretty-symbols)
  (setq pretty-symbol-categories `(lambda))
  (add-hook 'tc/common-coding-hooks 'pretty-symbols-mode))

(defun tc/run-common-coding-hooks ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'tc/common-coding-hooks))

(add-hook 'tc/lisp-coding-hooks 'tc/turn-on-paredit)
(add-hook 'tc/lisp-coding-hooks 'tc/turn-on-show-paren)
;;(add-hook 'tc/lisp-coding-hooks 'tc/turn-on-folding)
(add-hook 'tc/lisp-coding-hooks 'rainbow-delimiters-mode)


(defun tc/run-lisp-coding-hooks ()
  "Enable things that are convenient for lisp code."
  (run-hooks 'tc/lisp-coding-hooks))

(require 'imenu)

;; from emacs-starter-kit
(defun tc/ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;; C-x TAB
(global-set-key (kbd "C-x C-i") 'tc/ido-imenu)
(set-default 'imenu-auto-rescan t)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; notify when compilation completes
(require 'notifications)
(defun add-compile-notify-hook ()
  (add-to-list 'compilation-finish-functions
               (lambda (buf result)
                 (notifications-notify
                  :title "Compilation Finished" :body result))))

(add-hook 'compilation-mode-hook 'add-compile-notify-hook)

;; scroll compilation buffer until an error occurs
(setq compilation-scroll-output 'first-error)

(defun tc/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (let (start end)
    (if (region-active-p)
        (setq start (region-beginning)
              end (region-end))
      (setq start (point-min)
            end (point-max)))
    (untabify start end)
    (indent-region start end)
    (delete-trailing-whitespace start end)))

(global-set-key (kbd "C-c n") 'tc/cleanup-buffer)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c c") 'compile)
