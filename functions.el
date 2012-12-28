;; useful functions that don't belong elsewhere

;; useful for switching between buffers of mode
(defun tc/ido-for-mode(prompt the-mode)
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

