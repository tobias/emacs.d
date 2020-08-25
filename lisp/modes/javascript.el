(require 'js2-mode)

;; set up js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'"  . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun tc/js2-adjust-offset ()
  (setq js2-basic-offset 2))

;; it's a crime to indent 4 spaces
(add-hook 'js2-mode-hook 'tc/js2-adjust-offset)

;; it's a crime to not use paredit (except that it doesn't work well
;; with js, dangit)
;;(add-hook 'js2-mode-hook 'tc/turn-on-paredit-nonlisp)

;; get some jshint action going
(add-hook 'js2-mode-hook 'tc/turn-on-flycheck)

(add-hook 'js2-mode-hook 'tc/run-common-coding-hooks)

;; make curlies better
(define-key js2-mode-map "{" 'paredit-open-curly)
(define-key js2-mode-map "}" 'paredit-close-curly-and-newline)

;; set up nvm
;; TODO: make this better, maybe read the correct version from config?
(when (file-directory-p (concat user-emacs-directory
                                "../.nvm/versions"))
  (nvm-use "v12.18.0"))
