(require 'projectile)

(projectile-mode)

(setq projectile-completion-system 'ivy)

(global-set-key (kbd "M-g s") 'projectile-grep)

(global-set-key (kbd "C-x y") 'projectile-find-file)
