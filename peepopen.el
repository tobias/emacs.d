(defun peepopen-goto-file-gui ()
  "Uses external GUI app to quickly jump to a file in the project."
  (interactive)
  (defun string-join (separator strings)
    "Join all STRINGS using SEPARATOR."
    (mapconcat 'identity strings separator))
  (let ((root (find-project-root)))
    (when (null root)
      (error
       (concat
        "Can't find a suitable project root")))
    (shell-command-to-string
     (format "open 'peepopen://%s?editor=%s'"
             (expand-file-name root)
             (invocation-name)))))


;; borrowed from full-ack, and modified
;; http://nschum.de/src/emacs/full-ack/
(defun find-project-root ()
  "A function to find the project root directory."
  (catch 'root
    (let ((dir (expand-file-name (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   default-directory)))
          (prev-dir nil))
      (while (not (equal dir prev-dir))
        (when (directory-files dir nil "^\\.\\(git\\|hg\\|svn\\)$" t)
          (throw 'root dir))
        (setq prev-dir dir
              dir (file-name-directory (directory-file-name dir)))))))

(global-set-key (kbd "C-x y") 'peepopen-goto-file-gui)

