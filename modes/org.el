(setq org-hide-leading-stars t)

(require 'ox-reveal)
;(setq org-reveal-root "file:///home/tcrawley/hack/reveal.js-2.5.0/")
(setq org-reveal-root "./")

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
