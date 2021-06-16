;; from https://www.emacswiki.org/emacs/HeaderLine
(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(setq
 tc/header-drop-str (with-face "[...]"
                               :background "blue"
                               :weight 'bold)
 tc/header-modified-msg (with-face " (modified)"
                                   :foreground "yellow"
                                   :weight 'bold
                                   :slant 'italic))

(defun tc/make-header ()
  "Generates a buffer header with the filename and modified status, truncating to fit."
  (let* ((abbrev-file-name (abbreviate-file-name buffer-file-name))
         (file-dir (file-name-directory abbrev-file-name))
         (avail-width (if (buffer-modified-p)
                       (- (window-body-width) (length tc/header-modified-msg))
                     (window-body-width)))
         (new-header (if (> (length abbrev-file-name) avail-width)
                         (if (> (length file-dir) avail-width)
                             (concat tc/header-drop-str
                                     (with-face (substring file-dir
                                                           (+ (- (length file-dir)
                                                                 avail-width)
                                                              (length tc/header-drop-str))
                                                           (length file-dir))
                                                :weight 'bold))
                           (concat (with-face file-dir
                                              :foreground "#8fb28f"
                                              :weight 'bold)))
                       (concat (with-face file-dir
                                          :weight 'bold
                                          :foreground "#8fb28f")
                               (with-face (file-name-nondirectory buffer-file-name)
                                          :weight 'bold)))))
    (if (buffer-modified-p)
        (concat new-header tc/header-modified-msg)
      new-header)))

(defun headerable-buffer ()
  (not (eq 'erc-mode major-mode)))

(defun tc/display-header ()
  (when (headerable-buffer)
    (setq header-line-format
            '("" ;; invocation-name
              (:eval (if (buffer-file-name)
                         (tc/make-header)
                       "%b"))))))

(add-hook 'buffer-list-update-hook
          'tc/display-header)
