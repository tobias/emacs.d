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
     (format "open -a PeepOpen '%s'"
             (expand-file-name root)))))


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
        (when (directory-files dir nil "\\`.git\\'" t)
          (throw 'root dir))
        (setq prev-dir dir
              dir (file-name-directory (directory-file-name dir)))))))

(global-set-key (kbd "C-x t") 'peepopen-goto-file-gui)

