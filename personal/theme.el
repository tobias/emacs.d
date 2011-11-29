;(load "lib/color-theme-black-on-gray-modified")
;(color-theme-black-on-gray-modified)

(add-to-list 'load-path (concat user-emacs-directory "vendor/emacs-color-theme-solarized"))
(load "color-theme-solarized")
(color-theme-solarized-dark)

(when window-system
  (if (eq system-type 'darwin)
      (set-face-font `default "-apple-inconsolata-medium-r-normal--16-0-72-72-m-0-iso10646-1")
    (set-face-font `default "Inconsolata-10")))








     
 


       
