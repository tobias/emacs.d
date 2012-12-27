(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "lib"))

;; keep customize settings in their own file
(setq custom-file (concat user-emacs-directory "personal/custom.el"))
(load custom-file 'noerror)

(require 'epa-file)

(load "private.el.gpg")
(load "personal/env")
(load "personal/packages")
(load "personal/settings")
(load "personal/defuns")
(load "personal/bindings")
(load "personal/theme")
(load "personal/ruby")
(load "personal/java")
(load "personal/clojure")
(load "personal/completion")
(load "personal/ido")
(load "personal/mac")
(load "personal/nxml")
(load "personal/lisp")
(load "personal/irc")
(load "personal/mail")


