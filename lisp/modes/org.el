(setq org-hide-leading-stars t)

;;(require 'ox-reveal)
;;(setq org-reveal-root "file:///home/tcrawley/hack/reveal.js-2.5.0/")
;;(setq org-reveal-root "./")

(require 'ox-odt)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(add-hook 'before-save-hook 'time-stamp)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(setq org-plantuml-jar-path
      "/usr/local/Cellar/plantuml/1.2018.2/libexec/plantuml.jar")

(defvar clubhouse-link-regex
  "^https://app\\.clubhouse\\.io/[^/]+/\\(story\\|epic\\)/\\([0-9]+\\)/\\(.+\\)")

(defun tc/org-insert-link (url description)
  (insert "[[" url "][" description "]]"))

(defun org-insert-clubhouse-link (url)
  (interactive (list (let ((url (current-kill 0 t)))
                       (if (string-match clubhouse-link-regex url)
                           url
                         (read-string "Clubhouse URL: ")))))
  (when (null (string-match clubhouse-link-regex url))
    (error "Invalid Clubhouse URL '%s'" url))
  (let ((clubhouse-id (match-string 2 url)))
    (tc/org-insert-link url (concat "#" clubhouse-id))))

(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "DONE")))

(require 'org-daypage)

(setq daypage-path "~/Dropbox/journal/")

(defun tc/copy-task-to-next-daypage ()
  (interactive)
  (save-excursion
    (kill-ring-save (line-beginning-position)
                    (line-end-position))
    (daypage-next)
    (goto-char (point-min))
    (search-forward "* Tasks")
    (forward-line 2)
    (yank)
    (org-set-tags-to ":carry:")
    (org-align-all-tags)
    (insert "\n")
    (save-buffer)
    (daypage-prev)))

(define-key daypage-mode-map (kbd "C-c t n") 'tc/copy-task-to-next-daypage)
(define-key daypage-mode-map (kbd "<C-left>") 'daypage-prev)
(define-key daypage-mode-map (kbd "<C-right>") 'daypage-next)

(global-set-key "\C-con" 'todays-daypage)
(global-set-key "\C-coN" 'find-daypage)
