(setq my-config-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path my-config-dir)
(add-to-list 'load-path (concat my-config-dir "lib"))

;; keep customize settings in their own file
(setq custom-file (concat my-config-dir "personal/custom.el"))
(load custom-file 'noerror)

(defun load-personal()
  (load "personal/env")
  (load "personal/defuns")
  (load "personal/bindings")
  (load "personal/theme")
  (load "personal/ruby")
  (load "personal/tab-completion")
  (load "personal/ido")
  (load "personal/lisp"))

;; el-get
(add-to-list 'load-path (concat my-config-dir "el-get/el-get"))
(if (require 'el-get nil t)
    (progn
      (load "personal/packages")	
      (el-get 'sync)
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


;; el-get fails to load magit for some reason, so we do it manually
(add-to-list 'load-path (concat my-config-dir "el-get/magit"))
(load "magit")

