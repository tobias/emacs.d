(add-hook 'clojure-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'clojure-mode-hook 'tc/run-lisp-coding-hooks)

(add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl t)

(when (not tc/presentation-mode-p)
  (add-to-list 'pretty-symbol-patterns
               '(?λ lambda "(\\(fn\\)\\>" (clojure-mode nrepl-repl-mode) 1))
  (add-to-list 'pretty-symbol-patterns
               '(?λ lambda "\\(#\\)(" (clojure-mode nrepl-repl-mode) 1)))

(require 'nrepl)

(add-hook 'nrepl-repl-mode-hook 'tc/run-lisp-coding-hooks)

;; TODO: this needs cleanup
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (mapply 'defun)
     (cond-> 'defun)
     (cond->> 'defun)
     (go-loop 'defun)))


