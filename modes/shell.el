(require 'ansi-color)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun tc/ido-shell-buffer()
  (interactive)
  (tc/ido-for-mode "Shell:" 'shell-mode))
