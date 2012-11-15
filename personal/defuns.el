(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;;TODO: make a cleanup region using indent-region & untabify
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))


;; from http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))


(defun byte-recompile-home ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

;; from http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 3))))


(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun turn-on-paredit ()
  (paredit-mode t))

(defun turn-off-tool-bar ()
  (tool-bar-mode -1))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'coding-hook 'local-column-number-mode)
(add-hook 'coding-hook 'local-comment-auto-fill)
(add-hook 'coding-hook 'highlight-parentheses-mode)
(add-hook 'coding-hook 'turn-on-hl-line-mode)
(add-hook 'coding-hook 'turn-on-save-place-mode)
(add-hook 'coding-hook 'pretty-lambdas)
(add-hook 'coding-hook 'add-watchwords)
  
(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(defun ido-imenu ()
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

(defun open-trace-and-file (tracefile file linenum)
  "Open visit TRACEFILE in one window (in compilation mode), and visit FILE at LINENUM in another"
  (find-file-other-window tracefile)
  (compilation-mode)
  (goto-line 2)
  (find-file-other-window file)
  (goto-line linenum))

(defun yank-to-gist ()
  "yank from the top of the kill ring, create a gist from it, and insert the gist url at the point"
  (interactive)
  (save-excursion
    (let ((buffer (current-buffer)))
            (set-buffer (get-buffer-create "*yank-to-gist*"))
            (yank)
            (gist-region
             (point-min)
             (point-max)
             t
             (lexical-let ((buf buffer))
               (function (lambda (status)
                           (let ((location (cadr status)))
                             (set-buffer buf)
                             (message "Paste created: %s" location)
                             (insert location)
                             (kill-new location))))))
            (kill-buffer))))


(defun fix-tb-stack ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "at " nil t)
    (replace-match "\nat " nil t)))


 

(defun random-color-theme ()
  (interactive)
  (random t)
  (let ((theme (car (nth (random (length color-themes)) color-themes))))
    (message "Setting theme to %s" theme)
    (funcall theme)))

;;(random-color-theme)(run-with-timer 1 (* 60 60) 'random-color-theme)


;; (defvar *split-windows-list* '())

;; (defun switch-to-split-window (idx)
;;   (let ((win (nth idx *split-windows-list*)))
;;     (when win
;;       (select-window win))))

;; (defun win-split-3 ()
;;   (interactive)
;;   (delete-other-windows)
;;   (setq *split-windows-list*
;;         (list (selected-window)
;;               (select-window (split-window-horizontally))
;;               (split-window-vertically)))
;;   (select-window (first *split-windows-list*)))

;; (defun win-split-4 ()
;;   (interactive)
;;   (delete-other-windows)
;;   (let* ((top-left-w (selected-window))
;;          (center-w (split-window-horizontally (/ (frame-width) 3))))
;;     (setq *split-windows-list*
;;           (list top-left-w
;;                 (split-window-vertically)     ; bottom left
;;                 (select-window center-w)            
;;                 (split-window-horizontally))) ; right                 
;;     (select-window center-w)))

;; (defun win-split-5 ()
;;   (interactive)
;;   (delete-other-windows)
;;   (let* ((top-left-w (selected-window))
;;          (center-w (split-window-horizontally (/ (frame-width) 3))))
;;     (setq *split-windows-list*
;;           (list top-left-w
;;                 (split-window-vertically)                   ; bottom left
;;                 (select-window center-w)            
;;                 (select-window (split-window-horizontally)) ; top right
;;                 (split-window-vertically)))                 ;bottom right
;;     (select-window center-w)))



(defun ido-for-mode(prompt the-mode)
  (switch-to-buffer
   (ido-completing-read prompt
                        (save-excursion
                          (delq
                           nil
                           (mapcar (lambda (buf)
                                     (when (buffer-live-p buf)
                                       (with-current-buffer buf
                                         (and (eq major-mode the-mode)
                                              (buffer-name buf)))))
                                   (buffer-list)))))))

(defun ido-shell-buffer()
  (interactive)
  (ido-for-mode "Shell:" 'shell-mode))


(defun ido-erc-buffer()
  (interactive)
  (ido-for-mode "Channel:" 'erc-mode))
