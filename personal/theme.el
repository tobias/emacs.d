;(color-theme-topfunky)
;(color-theme-arjen)
;(color-theme-ir-black)
;(color-theme-hober2)
;(require 'color-theme-solarized)
;(color-theme-solarized-dark)
;(require 'zenburn)
;(zenburn)
(color-theme-feng-shui)

(when window-system
  (if (eq system-type 'darwin)
      (set-face-font `default "-apple-inconsolata-medium-r-normal--14-0-72-72-m-0-iso10646-1")
    (set-face-font `default "Inconsolata-10")))
