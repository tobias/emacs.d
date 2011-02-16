(require 'compile)

(defvar mvn-command-history nil
  "Maven command history variable")

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
                                     (concat "mvn -f " dir "pom.xml test") nil nil 'mvn-command-history)))))



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
