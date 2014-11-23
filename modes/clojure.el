(add-hook 'clojure-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'clojure-mode-hook 'tc/run-lisp-coding-hooks)
(add-hook 'clojure-mode-hook 'subword-mode)

(add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))

(require 'cider)

(setq
 clojure-defun-style-default-indent t
 cider-popup-stacktraces            t
 cider-auto-select-error-buffer     t
 cider-repl-print-length            100
 cider-repl-wrap-history            t
 cider-repl-history-file           (concat user-emacs-directory "cider-repl-history"))

(require 'clj-refactor)

(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "C-c C-m")))

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

(define-key cider-mode-map (kbd "C-c C-c") 'send-previous-expr-to-repl)

(require 'compile)

(defun tc/run-command (command prompt default-task filename history-var)
    (let* ((dirs (tc/locate-all-dominating-files default-directory filename))
         (dir (case (length dirs)
                (0 nil)
                (1 (first dirs))
                (t (ido-completing-read "Dir? " dirs)))))
    (if dir
        (compile (concat (format "cd %s;%s " dir command)
                         (read-from-minibuffer prompt default-task
                                               nil nil history-var)))
      (message "No %s found" filename))))

(defvar lein-history nil)

(defun tc/run-lein ()
  "Searches up the path for all project.clj's, asks at what level
to run the command (if more than one are found), then asks for a
lein command."
  (interactive)
  (tc/run-command "lein" "Lein task: " "install" "project.clj" 'lein-history))

(defvar boot-history nil)

(defun tc/run-boot ()
  "Searches up the path for all build.boot's, asks at what level
to run the command (if more than one are found), then asks for a
boot command."
  (interactive)
  (tc/run-command "boot" "Boot task: " "build" "build.boot" 'boot-history))

(define-key clojure-mode-map (kbd "C-c l") 'tc/run-lein)
(define-key clojure-mode-map (kbd "C-c b") 'tc/run-boot)

;; disable clojure-toggle-keyword-string, since the binding shadows
;; ace-window and I never use the binding
(define-key clojure-mode-map (kbd "C-:") nil)
