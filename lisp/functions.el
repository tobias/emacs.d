;; useful functions that don't belong elsewhere

(defun tc/toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

(defun tc/buffers-for-mode (the-mode)
  "Returns a list of all buffers with the given major mode."
  (delq
   nil
   (mapcar (lambda (buf)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (and (eq major-mode the-mode)
                      (buffer-name buf)))))
           (buffer-list))))

;; useful for switching between buffers of mode
(defun tc/ido-for-mode (prompt the-mode)
  (switch-to-buffer
   (ido-completing-read prompt
                        (save-excursion
                          (tc/buffers-for-mode the-mode)))))

;; tools for getting data from authinfo and friends

(defun tc/get-auth-value (host port key)
  (let ((result (auth-source-search :host host :port port)))
    (if result
        (plist-get (nth 0 result) key))))

(defun tc/get-auth-password (host port)
  (let ((pw (tc/get-auth-value host port :secret)))
    (if (functionp pw)
        (funcall pw)
      pw)))

;; from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun tc/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun tc/locate-all-dominating-files (dir filename)
  "Searches for FILENAME in DIR and its parents, returning a list
 of all dirs containing the file."
  (let ((found-dir (locate-dominating-file (expand-file-name dir) filename)))
    (if found-dir
        (cons found-dir (tc/locate-all-dominating-files
                         (concat found-dir "..")
                         filename)))))

;; from http://ergoemacs.org/emacs/elisp_hash_table.html
(defun tc/extract-hash-keys (hashtable)
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys))

(defun tc/tmp-buffer (suffix)
  "Creates a tmp buffer with the given suffix"
  (find-file (concat "/tmp/" (number-to-string (random)) "." suffix)))

(defun tc/tmp-md ()
  "Creates a tmp markdown buffer"
  (interactive)
  (tc/tmp-buffer ".md"))

(defun tc/standup ()
  "Creates a new buffer with a standup template inserted"
  (interactive)
  (find-file (concat user-emacs-directory "/../.standup-template.md"))
  (set-visited-file-name (concat "/tmp/" "standup" (number-to-string (random)) ".md"))
  (goto-char (point-min)))

(defun tc/generate-jira-url ()
  "Takes the thing at point places it at the end of a url string, putting the result on the kill ring"
  (interactive)
  (kill-new (concat "https://farmlogs.atlassian.net/browse/" (thing-at-point 'symbol))))
