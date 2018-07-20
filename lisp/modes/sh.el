(defun tc/sh-indent ()
  (setq sh-basic-offset 2
        sh-indentation 2))

(add-hook 'sh-mode-hook 'tc/sh-indent)
