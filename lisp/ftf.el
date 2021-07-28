(use-package find-things-fast)

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

;; override find command generation to ignore ftf-filetypes. I want to
;; consider every file. Maybe ftf is no longer the best tool for this
;; job

(defun ftf-get-find-command ()
  "Creates the raw, shared find command."
  "find . -path '*/.svn' -prune -o -path '*/.hg' -prune -o -name '*'")

(defun ftf-project-files-string ()
  "Returns a string with the raw output of ."
  (let ((git-toplevel (ftf-get-top-git-dir default-directory)))
    (cond (git-toplevel
           (shell-command-to-string
            "git ls-files"))
           (t
            (let ((default-directory (ftf-project-directory)))
              (shell-command-to-string (ftf-get-find-command)))))))

(defun ftf-grepsource (cmd-args)
  "Greps the current project, leveraging local repository data
for speed and falling back on a big \"find | xargs grep\"
command if we aren't.

The project's scope is defined first as a directory containing
either a `.dir-locals.el' file or an `.emacs-project' file OR the
root of the current git or mercurial repository OR a project root
defined by the optional `project-root.el' package OR the default
directory if none of the above is found."
  (interactive (ftf-interactive-default-read "Grep project for string: "))
  ;; When we're in a git repository, use git grep so we don't have to
  ;; find-files.
  (let ((quoted (replace-regexp-in-string "\"" "\\\\\"" cmd-args))
        (git-toplevel (ftf-get-top-git-dir default-directory))
        (default-directory (ftf-project-directory))
        (null-device nil))
    (cond (git-toplevel ;; We can accelerate our grep using the git data.
           (grep (concat "git --no-pager grep --no-color -n -e \""
                         quoted
                         "\"")))
          (t            ;; Fallback on find|xargs
             (grep (concat (ftf-get-find-command)
                           " | xargs grep -nH -e \"" quoted "\""))))))

;; end ftf overrides

(global-set-key (kbd "C-x y") 'ftf-find-file)
(global-set-key (kbd "M-g s") 'ftf-grepsource)
