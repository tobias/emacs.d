(setq org-hide-leading-stars t)

;;(require 'ox-reveal)
;;(setq org-reveal-root "file:///home/tcrawley/hack/reveal.js-2.5.0/")
;;(setq org-reveal-root "./")

(require 'ox-odt)
(setq org-odt-preferred-output-format "docx")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(add-hook 'before-save-hook 'time-stamp)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (restclient . t)))

(setq org-plantuml-jar-path
      "/usr/local/Cellar/plantuml/1.2018.2/libexec/plantuml.jar")

(setq clubhouse-link-regex
  "^https://app[^.]*\\.clubhouse\\.io/[^/]+/\\(story\\|epic\\)/\\([0-9]+\\)")

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
    (tc/org-insert-link url (concat "ch" clubhouse-id))))

;; (setq org-todo-keywords
;;       '((sequence "☐" "☆" "☒")))
;; ☑ ☒

(setq org-todo-keywords       '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "PENDING(p)" "TOMERGE(m)" "|" "DONE(d)" "CANCELED(c)"))
      org-ellipsis            "⁙"
      org-log-done            'time
      ;; org-bullets-bullet-list '("⁂")
      )

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; (require 'org-daypage)

;; (setq daypage-path "~/Dropbox/journal/")

;; (define-key daypage-mode-map (kbd "<C-left>") 'daypage-prev)
;; (define-key daypage-mode-map (kbd "<C-right>") 'daypage-next)

;; (global-set-key "\C-con" 'todays-daypage)
;; (global-set-key "\C-coN" 'find-daypage)

(require 'org-weekpage)

(setq weekpage-path "~/Dropbox/journal/")

(define-key weekpage-mode-map (kbd "<C-left>") 'weekpage-prev)
(define-key weekpage-mode-map (kbd "<C-right>") 'weekpage-next)
;; override compile
(define-key weekpage-mode-map (kbd "C-c c") 'org-ctrl-c-ctrl-c)
;; (global-unset-key "\C-co")
(global-set-key "\C-con" 'this-weeks-weekpage)
(global-set-key "\C-coN" 'find-weekpage)
