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
(add-hook 'coding-hook 'turn-on-hl-line-mode)
(add-hook 'coding-hook 'turn-on-save-place-mode)
(add-hook 'coding-hook 'pretty-lambdas)
(add-hook 'coding-hook 'add-watchwords)
  
(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

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


 

