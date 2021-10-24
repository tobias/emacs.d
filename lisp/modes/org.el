(use-package org
  :init
  (setq org-hide-leading-stars t)
  :hook
  ;; Make windmove work in org-mode:
  (org-shiftup-final-hook . windmove-up)
  (org-shiftleft-final-hook . windmove-left)
  (org-shiftdown-final-hook . windmove-down)
  (org-shiftright-final-hook . windmove-right))

;;(require 'ox-reveal)
;;(setq org-reveal-root "file:///home/tcrawley/hack/reveal.js-2.5.0/")
;;(setq org-reveal-root "./")

(use-package ox-odt
  :init
  (setq org-odt-preferred-output-format "docx"))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(add-hook 'before-save-hook 'time-stamp)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (restclient . t)))

(setq org-plantuml-jar-path
      ;; linux
      "/usr/share/plantuml/plantuml.jar"
      ;; macos
      ;;"/usr/local/Cellar/plantuml/1.2018.2/libexec/plantuml.jar"
      )

(setq shortcut-link-regex
  "^https://app[^.]*\\.\\(clubhouse\\.io\\|shortcut\\.com\\)/[^/]+/\\(story\\|epic\\)/\\([0-9]+\\)")

(defun tc/org-insert-link (url description)
  (insert "[[" url "][" description "]]"))

(defun org-insert-shortcut-link (url)
  (interactive (list (let ((url (current-kill 0 t)))
                       (if (string-match shortcut-link-regex url)
                           url
                         (read-string "Shortcut URL: ")))))
  (when (null (string-match shortcut-link-regex url))
    (error "Invalid Shortcut URL '%s'" url))
  (let ((shortcut-id (match-string 3 url)))
    (tc/org-insert-link url (concat "sc-" shortcut-id))))

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "PENDING(p)" "TOMERGE(m)" "|" "DONE(d)" "CANCELED(c)"))
      org-ellipsis      "⁙")

(use-package org-bullets
  :hook
  (org-mode-hook . org-bullets-mode))

(require 'org-weekpage)

(setq weekpage-path "~/Dropbox/journal/")

(define-key weekpage-mode-map (kbd "<C-left>") 'weekpage-prev)
(define-key weekpage-mode-map (kbd "<C-right>") 'weekpage-next)
;; override compile
(define-key weekpage-mode-map (kbd "C-c c") 'org-ctrl-c-ctrl-c)
(global-unset-key "\C-co")
(global-set-key "\C-con" 'this-weeks-weekpage)
(global-set-key "\C-coN" 'find-weekpage)
