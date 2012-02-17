(setq user-emacs-directory (file-name-directory (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "lib"))

;; keep customize settings in their own file
(setq custom-file (concat user-emacs-directory "personal/custom.el"))
(load custom-file 'noerror)

(load "private")
(load "personal/env")

(defun load-personal()
   (load "personal/settings")
   (load "personal/defuns")
   (load "personal/bindings")
   (load "personal/theme")
   (load "personal/ruby")
   (load "personal/java")
   (load "personal/clojure")
   (load "personal/completion")
   (load "personal/ido")
   (load "personal/mac")
   (load "personal/nxml")
   (load "personal/lisp")
   (load "personal/irc"))

;; el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(if (require 'el-get nil t)
    (progn
      (load "personal/packages")	
      (el-get 'sync my-packages)
      (load-personal))			
  (progn
    (message "We need to install el-get")
    (url-retrieve
     "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
     (lambda (s)
       (end-of-buffer)
       (eval-print-last-sexp)
       (load "personal/packages")
       (el-get 'sync)
       (load-personal)))))


(add-to-list 'load-path (concat user-emacs-directory "vendor/magit"))
(load "magit")
;(load "slime")

