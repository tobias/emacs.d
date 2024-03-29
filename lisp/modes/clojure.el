;; use word wrapping instead of truncation in test report and doc
;; buffers. Must be set before loading cider.
(setq cider-special-mode-truncate-lines nil)

(require 'cider)
(require 'clj-refactor)
;;(require 'align-cljlet)
;;(require 'flycheck-joker)
(require 'flycheck-clj-kondo)
(require 'cljstyle-mode)

(when shortcut-elisp-loaded
  (shortcut-backend-enable-matcher-combinator-test-output-colors))

(defun tc/rename-buffer-to-ns ()
  (interactive)
  (let ((ns (clojure-expected-ns)))
    (when (not (string= "" ns))
      (rename-buffer ns))))

(add-hook 'clojure-mode-hook 'tc/rename-buffer-to-ns)
(add-hook 'clojure-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'clojure-mode-hook 'tc/run-lisp-coding-hooks)
(add-hook 'clojure-mode-hook 'tc/turn-on-flycheck)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'yas-minor-mode)
(when shortcut-elisp-loaded
  (add-hook 'clojure-mode-hook 'shortcut-backend-font-lock))

(add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

(setq
 clojure-indent-style               :always-align

 cider-font-lock-dynamically        nil
 cider-popup-stacktraces            t
 cider-auto-select-error-buffer     t
 ;; try symbol at point before asking
 cider-prompt-for-symbol            nil
 cider-eval-spinner-type            'vertical-breathing

 cider-repl-print-length            100
 cider-repl-wrap-history            t
 cider-repl-history-file           (concat user-emacs-directory "cider-repl-history")
 cider-repl-pop-to-buffer-on-connect 'display-only
 cider-repl-use-content-types       nil
 cider-repl-display-help-banner     nil
 cider-repl-prompt-function         (lambda (namespace)
                                      (format "%s>\n" namespace)))

(defun tc/turn-on-clj-refactor ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (define-key clj-refactor-map (kbd "C-c C-m al") 'align-cljlet))

(add-hook 'clojure-mode-hook 'tc/turn-on-clj-refactor)

;; (when (not tc/presentation-mode-p)
;;   (add-to-list 'pretty-symbol-patterns
;;                '(?λ lambda "(\\(fn\\)\\>" (clojure-mode cider-repl-mode) 1))
;;   (add-to-list 'pretty-symbol-patterns
;;                '(?λ lambda "\\(#\\)(" (clojure-mode cider-repl-mode) 1)))

(add-hook 'cider-repl-mode-hook 'tc/run-lisp-coding-hooks)
(add-hook 'cider-repl-mode-hook 'subword-mode)

;; run cljstyle on file. I don't use cljstyle-mode to do this
;; automatically on save since it causes the buffer to jump and breaks
;; compilation buffer references in the file
(define-key clojure-mode-map (kbd "C-c TAB") 'cljstyle)

(defun tc/insert-divider-comment ()
  (interactive)
  (move-beginning-of-line nil)
  (insert ";; -----  -----\n")
  (previous-line)
  (search-forward "- "))

(define-key clojure-mode-map (kbd "C-c d") 'tc/insert-divider-comment)

(defun tc/insert-comment (text)
  (move-beginning-of-line nil)
  (insert (format ";; %s" text))
  (indent-for-tab-command))

(defun tc/insert-note (type)
  (tc/insert-comment (format "%s: (toby) " type)))

(defun tc/insert-fixme ()
  (interactive)
  (tc/insert-note "FIXME"))

(defun tc/insert-todo ()
  (interactive)
  (tc/insert-note "TODO"))

(defun tc/insert-nocommit ()
  (interactive)
  (tc/insert-note "NOCOMMIT"))

(defun tc/insert-given ()
  (interactive)
  (tc/insert-comment "Given: "))

(defun tc/insert-when ()
  (interactive)
  (tc/insert-comment "When: "))

(defun tc/insert-then ()
  (interactive)
  (tc/insert-comment "Then: "))

(defun tc/insert-and ()
  (interactive)
  (tc/insert-comment "And: "))

(define-key clojure-mode-map (kbd "C-c C-n f") 'tc/insert-fixme)
(define-key clojure-mode-map (kbd "C-c C-n t") 'tc/insert-todo)
(define-key clojure-mode-map (kbd "C-c C-n c") 'tc/insert-nocommit)
(define-key clojure-mode-map (kbd "C-c C-n g") 'tc/insert-given)
(define-key clojure-mode-map (kbd "C-c C-n w") 'tc/insert-when)
(define-key clojure-mode-map (kbd "C-c C-n n") 'tc/insert-then)
(define-key clojure-mode-map (kbd "C-c C-n a") 'tc/insert-and)

(when shortcut-elisp-loaded
  ;; use my local cider alias and set up scope-capture
  (setq shortcut-backend-default-clojure-cli-options
      "-J-server -J-Xmx8g -J-XX:+UseG1GC -J-Dapple.awt.UIElement=true -J-Dtika.config=tika-config.xml -M:backend-defaults:dev:test:cider")
  (add-to-list 'cider-jack-in-nrepl-middlewares "sc.nrepl.middleware/wrap-letsc")
  (cider-add-to-alist 'cider-jack-in-dependencies "vvvvalvalval/scope-capture-nrepl" "0.3.1"))


(defun tc/insert-spy ()
  (interactive)
  (move-beginning-of-line nil)
  (insert "(sc.api/spy)")
  (indent-for-tab-command))

(defun tc/insert-spy-letsc ()
  (interactive)
  (insert "(sc.api/letsc )")
  (backward-char))

(define-key clojure-mode-map (kbd "C-c s") 'tc/insert-spy)
(define-key cider-repl-mode-map (kbd "C-c s") 'tc/insert-spy-letsc)

;; return inserts newline, C-return or C-c return submits
(define-key cider-repl-mode-map (kbd "RET") 'indent-new-comment-line)
(define-key cider-repl-mode-map (kbd "C-RET") 'cider-repl-return)
(define-key cider-repl-mode-map (kbd "C-c RET") 'cider-repl-return)

(defun run-cider-command (command)
  (with-current-buffer (cider-current-repl-buffer)
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


;; copied from clojure-mode with redisplay calls commented out to
;; prevent buffer jumping when used as a before-save-hook
(defun tc/clojure-sort-ns ()
  "Internally sort each sexp inside the ns form."
  (interactive)
  (comment-normalize-vars)
  (if (clojure-find-ns)
      (save-excursion
        (goto-char (match-beginning 0))
        ;;        (redisplay)
        (let ((beg (point))
              (ns))
          (forward-sexp 1)
          (setq ns (buffer-substring beg (point)))
          (forward-char -1)
          (while (progn (forward-sexp -1)
                        (looking-at "(:[a-z]"))
            (save-excursion
              (forward-char 1)
              (forward-sexp 1)
              (clojure--sort-following-sexps)))
          (goto-char beg)
          (if (looking-at (regexp-quote ns))
              (message "ns form is already sorted")
            (sleep-for 0.1)
            ;;           (redisplay)
            (message "ns form has been sorted")
            (sleep-for 0.1))))
    (user-error "Namespace not found")))

(defun tc/turn-on-sorting-on-save ()
  (add-hook 'before-save-hook 'tc/clojure-sort-ns nil 'local))

;;(add-hook 'clojure-mode-hook 'tc/turn-on-sorting-on-save)

(require 'compile)

(defun tc/run-command (command prompt default-task filename history-var custom-buffer-name-p)
  (let* ((dirs (tc/locate-all-dominating-files default-directory filename))
         (dir (case (length dirs)
                (0 nil)
                (1 (first dirs))
                (t (ido-completing-read "Dir? " dirs)))))
      (if dir
          (let* ((task (read-from-minibuffer prompt default-task
                                             nil nil history-var))
                 (orig-comp-buf-name compilation-buffer-name-function)
                 (comp-buffer-name (format "*%s-%s-%s*"
                                           (file-name-nondirectory (directory-file-name dir))
                                           command
                                           task)))
            (when custom-buffer-name-p
              (message "%s" comp-buffer-name)
              (setq compilation-buffer-name-function
                    (lambda (_) comp-buffer-name)))
            (compile (format "cd %s;%s %s" dir command task))
            (setq compilation-buffer-name-function orig-comp-buf-name))
        (message "No %s found" filename))))

(defvar lein-history nil)

(defun tc/run-lein ()
  "Searches up the path for all project.clj's, asks at what level
to run the command (if more than one are found), then asks for a
lein command."
  (interactive)
  (tc/run-command "lein" "Lein task: " "install" "project.clj" 'lein-history current-prefix-arg))

(defvar boot-history nil)

(defun tc/run-boot ()
  "Searches up the path for all build.boot's, asks at what level
to run the command (if more than one are found), then asks for a
boot command."
  (interactive)
  (tc/run-command "boot" "Boot task: " "build" "build.boot" 'boot-history current-prefix-arg))

(define-key clojure-mode-map (kbd "C-c l") 'tc/run-lein)
(define-key clojure-mode-map (kbd "C-c b") 'tc/run-boot)

;; disable clojure-toggle-keyword-string, since the binding shadows
;; ace-window and I never use the feature
(define-key clojure-mode-map (kbd "C-:") nil)
