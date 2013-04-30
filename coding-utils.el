(require 'flymake-cursor)
(require 'linum)
(require 'diminish)

;; replace 'lambda' with a lambda symbol - if it's working, this comment makes
;; no sense
(defun tc/pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

;; replace fn with function symbol
(defun tc/pretty-fn ()
  (font-lock-add-keywords
   nil `(("(\\(\\<fn\\>\\)"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    "\u0192"
                                    'decompose-region)))))))

(defun tc/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (diminish 'auto-fill-function))

(defun tc/turn-on-whitespace ()
  (whitespace-mode t))

(defun tc/turn-on-paredit ()
  (paredit-mode t)
  (diminish 'paredit-mode))

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

(add-hook 'tc/common-coding-hooks 'tc/turn-on-idle-highlight)
(add-hook 'tc/common-coding-hooks 'linum-on)
(add-hook 'tc/common-coding-hooks 'tc/local-comment-auto-fill)
(add-hook 'tc/common-coding-hooks 'tc/pretty-lambdas)
(add-hook 'tc/common-coding-hooks 'tc/add-watchwords)
  
(defun tc/run-common-coding-hooks ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'tc/common-coding-hooks))

(add-hook 'tc/lisp-coding-hooks 'tc/turn-on-paredit)
(add-hook 'tc/lisp-coding-hooks 'tc/turn-on-show-paren)
(add-hook 'tc/lisp-coding-hooks 'tc/turn-on-folding)

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

(defun tc/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun tc/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;;TODO: make a cleanup region using indent-region & untabify
(defun tc/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (tc/indent-buffer)
  (tc/untabify-buffer)
  (delete-trailing-whitespace))

(global-set-key (kbd "C-c n") 'tc/cleanup-buffer)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c c") 'compile)

