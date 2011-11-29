(add-hook 'clojure-mode-hook 'linum-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(defun clj-slime ()
  "Run slime-connect with defaults"
  (interactive)
  (slime-connect "localhost" 4005))

(eval-after-load 'clojure-mode
  '(define-clojure-indent
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


