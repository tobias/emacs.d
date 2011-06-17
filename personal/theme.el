;(color-theme-topfunky)
;(color-theme-arjen)
;(color-theme-ir-black)
;(color-theme-hober2)
;(require 'color-theme-sanityinc-solarized)
;(color-theme-sanityinc-solarized-light)
;(require 'zenburn)
;(zenburn)
;(color-theme-feng-shui)
;(color-theme-pok-wog)
(load "personal/color-theme-pik-pok")
(color-theme-pik-pok)


(when window-system
  (if (eq system-type 'darwin)
      (set-face-font `default "-apple-inconsolata-medium-r-normal--14-0-72-72-m-0-iso10646-1")
    (set-face-font `default "Inconsolata-10")))

;; (defadvice mode-line-toggle-modified
;;   (before set-fringe-modified activate)
;;   (message "mltm called %s" (buffer-modified-p)))    

;; (defadvice buffer-modified-tick
;;   (before set-fringe-modified activate)
;;   (message "bmt called %s" (buffer-modified)))       







     
 


       
