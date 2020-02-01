;; from https://www.emacswiki.org/emacs/HeaderLine
(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun tc/make-header ()
  ""
  (let* ((tc/full-header (abbreviate-file-name buffer-file-name))
         (tc/header (file-name-directory tc/full-header))
         (tc/drop-str "[...]"))
    (if (> (length tc/full-header)
           (window-body-width))
        (if (> (length tc/header)
               (window-body-width))
            (progn
              (concat (with-face tc/drop-str
                                 :background "blue"
                                 :weight 'bold)
                      (with-face (substring tc/header
                                            (+ (- (length tc/header)
                                                  (window-body-width))
                                               (length tc/drop-str))
                                            (length tc/header))
                                 :weight 'bold)))
          (concat (with-face tc/header
                             :foreground "#8fb28f"
                             :weight 'bold)))
      (concat (with-face tc/header
                         :weight 'bold
                         :foreground "#8fb28f")
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold)))))

(defun tc/display-header ()
  (setq header-line-format
        '("" ;; invocation-name
          (:eval (if (buffer-file-name)
                     (tc/make-header)
                   "%b")))))

(add-hook 'buffer-list-update-hook
          'tc/display-header)
