(require 'ruby-mode)

;; Rake files are ruby, too, as are gemspecs, rackup files, and gemfiles.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(setq ruby-use-encoding-map nil)

;;(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'tc/run-common-coding-hooks)
;;x(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)

(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake)

     ;; Invoke ruby with '-c' to get syntax checking
     (defun tc/flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" tc/flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" tc/flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name)
                            (if (fboundp 'tramp-list-remote-buffers)
                                (not (subsetp
                                      (list (current-buffer))
                                      (tramp-list-remote-buffers)))
                              t))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))

(require 'compile)

(defvar rake-history nil)

(defun rake ()
  "Searches up the path for all Rakefile's, asks at what level
to run the command (if more than one are found), then asks for a
lein command."
  (interactive)
  (let* ((dirs (tc/locate-all-dominating-files default-directory "Rakefile"))
         (dir (case (length dirs)
                (0 nil)
                (1 (first dirs))
                (t (ido-completing-read "Project? " dirs)))))
    (if dir
        (compile (concat (format "cd %s;rake " dir)
                         (read-from-minibuffer "Rake task: " ""
                                               nil nil 'rake-history)))
      (message "No Rakefile found"))))

(define-key ruby-mode-map (kbd "C-c r") 'rake)

(require 'inf-ruby)

(defun tc/inf-ruby-send-line ()
  "Sends the current line to the inferior Ruby process."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (ruby-send-region (point) end))))

;; overrides ruby-load-file
(define-key inf-ruby-minor-mode-map (kbd "C-c C-l") 'tc/inf-ruby-send-line)

;; overrides inf-ruby
(define-key inf-ruby-minor-mode-map (kbd "C-c C-s") 'ruby-switch-to-inf)
