(require 'find-file-in-project)

(setq ffip-use-rust-fd t)

(global-set-key (kbd "C-x y") 'find-file-in-project)

(global-set-key (kbd "M-g s") 'ag-project)
