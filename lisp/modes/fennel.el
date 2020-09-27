(require 'fennel-mode)

(add-hook 'fennel-mode-hook 'tc/run-common-coding-hooks)
(add-hook 'fennel-mode-hook 'tc/run-lisp-coding-hooks)

