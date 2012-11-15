(add-hook 'clojure-mode-hook 'linum-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(defun clj-slime ()
  "Run slime-connect with defaults"
  (interactive)
  (slime-connect "localhost" 4005))

(add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))

(setq nrepl-popup-stacktraces nil)

(eval-after-load 'clojure-mode
  '(define-clojure-indent
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


