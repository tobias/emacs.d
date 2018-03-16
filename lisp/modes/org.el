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
