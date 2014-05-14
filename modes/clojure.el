(add-hook 'clojure-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'clojure-mode-hook 'tc/run-lisp-coding-hooks)
(add-hook 'clojure-mode-hook 'subword-mode)

(add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx$" . clojure-mode))

(require 'cider)

(setq
 clojure-defun-style-default-indent t
 cider-popup-stacktraces            nil
 cider-auto-select-error-buffer     t
 cider-repl-print-length            100
 cider-repl-wrap-history            t
 cider-repl-history-file           (concat user-emacs-directory "cider-repl-history"))

(when (not tc/presentation-mode-p)
  (add-to-list 'pretty-symbol-patterns
               '(?λ lambda "(\\(fn\\)\\>" (clojure-mode cider-repl-mode) 1))
  (add-to-list 'pretty-symbol-patterns
               '(?λ lambda "\\(#\\)(" (clojure-mode cider-repl-mode) 1)))


(add-hook 'cider-repl-mode-hook 'tc/run-lisp-coding-hooks)
(add-hook 'cider-repl-mode-hook 'subword-mode)

(defun run-cider-command (command)
  (with-current-buffer (cider-find-or-create-repl-buffer)
    (goto-char (point-max))
    (insert-char ?\n)
    (insert command)
    (cider-repl-return)))

(defun send-expr-to-repl ()
  (interactive)
  (run-cider-command (cider-sexp-at-point)))

(defun send-previous-expr-to-repl ()
  (interactive)
  (run-cider-command (cider-last-sexp)))

(define-key cider-mode-map (kbd "C-c C-c") 'send-expr-to-repl)

(require 'compile)

(defvar lein-history nil)

(defun lein ()
  "Searches up the path for all project.clj's, asks at what level
to run the command (if more than one are found), then asks for a
lein command."
  (interactive)
  (let* ((dirs (tc/locate-all-dominating-files default-directory "project.clj"))
         (dir (case (length dirs)
                (0 nil)
                (1 (first dirs))
                (t (ido-completing-read "Project? " dirs)))))
    (if dir
        (compile (concat (format "cd %s;lein " dir)
                         (read-from-minibuffer "Lein task: " "install"
                                               nil nil 'lein-history)))
      (message "No project.clj found"))))

(define-key clojure-mode-map (kbd "C-c l") 'lein)
(define-key clojure-mode-map (kbd "C-:") nil)
