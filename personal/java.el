(require 'compile)

(defun mvn(&optional args) 
  "Searches up the path for a pom.xml"
  (interactive)
  (let* ((dir (file-name-as-directory (expand-file-name default-directory)))
		 (found (file-exists-p (concat dir "pom.xml"))))
    (while (and (not found) (not (equal dir "/")))
      (setq dir (file-name-as-directory (expand-file-name (concat dir "..")))
            found (file-exists-p (concat dir "pom.xml"))))
    (if (not found)
        (message "No pom.xml found")
      (compile (read-from-minibuffer "Command: " 
                                     (concat "mvn -f " dir "pom.xml install") nil nil 'compile-history)))))



;;; For maven 2/3 output
(add-to-list 'compilation-error-regexp-alist
             '("^.*?\\(/.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3))

;;; TAGS setup
;; (setq tags-table-list '(
;;                         "~/src/torquebox"
;;                         "~/src/jboss-deployers"
;;                         "~/src/vfs/tags/3.0.0.CR5"
;;                         "~/local/java/src"
;;                         ))
;; (setq tags-revert-without-query 't)

;; (add-hook 'java-mode-hook 'jtags-mode)
;; (autoload 'jtags-mode "jtags")

;(define-key java-mode-map (kbd "C-c m") 'mvn)
(global-set-key (kbd "C-c m") 'mvn)

(add-hook 'java-mode-hook 'linum-mode)

(add-hook
 'java-mode-hook
 '(lambda () "Treat Java 1.5 @-style annotations as comments."
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; (add-to-list 'load-path (concat user-emacs-directory "lib/malabar-1.5-SNAPSHOT/lisp"))

;; (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;;                                       global-semanticdb-minor-mode
;;                                       global-semantic-idle-summary-mode
;;                                       global-semantic-mru-bookmark-mode))
;; (semantic-mode 1)
;; (require 'malabar-mode)
;; (setq malabar-groovy-lib-dir (concat user-emacs-directory "lib/malabar-1.5-SNAPSHOT/lib"))
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; (add-to-list 'load-path (concat user-emacs-directory "lib/jdibug-0.5"))

;; (require 'cedet)
;; (require 'jdibug)
