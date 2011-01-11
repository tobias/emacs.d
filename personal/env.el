(setenv "PATH" (shell-command-to-string "source ~/.path; echo -n $PATH"))
(setq exec-path (append exec-path (split-string (getenv "PATH") ":")))
