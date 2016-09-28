;; java related setup

(defun tc/annotations-as-comments ()
  "Treat Java 1.5 @-style annotations as comments."
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table))

(add-hook 'java-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'java-mode-hook 'tc/annotations-as-comments)

(require 'compile)

(defun mvn(&optional args)
  "Searches up the path for a pom.xml"
  (interactive)
  (let ((dir (locate-dominating-file default-directory "pom.xml")))
    (if (not dir)
        (message "No pom.xml found")
      (compile (read-from-minibuffer "Command: "
                                     (concat "mvn -f " dir "pom.xml install") nil nil 'compile-history)))))

;;; For maven 2/3 output
(add-to-list 'compilation-error-regexp-alist
             '("^.*?\\(/.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3))

(global-set-key (kbd "C-c m") 'mvn)

;; setup docbook schemas
(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files
                (concat user-emacs-directory "schemas/schemas.xml")))

