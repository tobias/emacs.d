(use-package find-file-in-project
  :init
  (setq ffip-use-rust-fd t)
  :bind
  (("C-x y" . find-file-in-project)
   ("M-g s" . ag-project)))
