(add-hook 'clojure-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'clojure-mode-hook 'tc/run-lisp-coding-hooks)
(add-hook 'clojure-mode-hook 'tc/pretty-fn)

(add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(setq nrepl-popup-stacktraces nil)

(require 'nrepl)

(defun nrepl-port-from-file ()
  (interactive)
  (let* ((dir (nrepl-project-directory-for (nrepl-current-dir)))
         (f (expand-file-name "target/repl-port" dir))
         (port (when (file-exists-p f)
                 (string-to-number
                  (with-temp-buffer
                    (insert-file-contents f)
                    (buffer-string))))))
    (if port
        (nrepl-connect "localhost" port)
      (message "No port file found"))))

;; TODO: this needs cleanup
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (mapply 'defun)
     (contract 'defun)
     (defconstrainedfn 'defun)
     (defcontract 'defun)
     (provide 'defun)
     (describe 'defun)
     (it 'defun)
     (before 'defun)
     (after 'defun)
     (before-all 'defun)
     (after-all 'defun)
     (with 'defun)
     (around 'defun)
     (should 'defun)
     (should-not 'defun)
     (should= 'defun)
     (should-not= 'defun)
     (should-fail 'defun)
     (should-throw 'defun)
     (should-not-throw 'defun)))


