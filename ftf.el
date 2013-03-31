(require 'find-things-fast)

;; override ftf defuns to show more path info
;; this should do a better job of just showing enough to be unique
(defun ftf-project-files-alist ()
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let ((table (ftf-project-files-hash))
        (default-directory (ftf-project-directory))
        file-alist)
    (maphash (lambda (file-name full-path)
               (cond ((> (length full-path) 1)
                      (dolist (path full-path)
                        (let ((entry (cons file-name path)))
                          (ftf-uniqueify default-directory entry)
                          (set 'file-alist (cons entry file-alist)))))
                     (t
                      (set 'file-alist
                           (cons (cons file-name (car full-path))
                                 file-alist)))))
             table)
    file-alist))

(defun ftf-uniqueify (root file-cons)
  "Set the car of the argument to include the directory name plus
the file name."
  (let ((file-name-len (length (car file-cons)))
        (root-len (length root)))
    (if (= root-len (- (length (cdr file-cons)) file-name-len))
        file-cons
      (setcar file-cons (concat (car file-cons) ": "
                                (substring (cdr file-cons)
                                           root-len
                                           (- -1 file-name-len)))))))
;; end ftf overrides

(global-set-key (kbd "C-x y") 'ftf-find-file)
(global-set-key (kbd "M-g s") 'ftf-grepsource)
