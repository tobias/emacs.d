;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "(...) ")
(setq-default dired-details-hide-link-targets nil)
(setq-default wdired-create-parent-directories t)
(dired-details-install)

(require 'dired-rainbow)
(require 'dired-collapse)

;; never use a brief listing
(global-set-key (kbd "C-x C-d") 'dired)
