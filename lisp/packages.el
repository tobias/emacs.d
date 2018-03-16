(when (or (string-prefix-p "24" emacs-version)
          (string-prefix-p "25" emacs-version))
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  (mapc (lambda (pkg)
          (add-to-list 'package-pinned-packages `(,pkg . "melpa-stable") t))
        '(cider clojure-mode))
  
  (package-initialize)
  ;;(package-refresh-contents)

  ;; (mapc (lambda (pkg)
  ;;         (and (not (package-installed-p pkg))
  ;;              (package-install pkg)))
  ;;       '(ace-window
  ;;         adoc-mode
  ;;         align-cljlet
  ;;         auto-complete
  ;;         browse-kill-ring
  ;;         cider
  ;;         cl-lib
  ;;         clj-refactor
  ;;         clojure-mode
  ;;         coffee-mode
  ;;         connection
  ;;         dash
  ;;         dictionary
  ;;         diminish
  ;;         dired-details
  ;;         ;; downplay-mode
  ;;         erc-hl-nicks
  ;;         ;;erc-image
  ;;         ;;erc-tweet
  ;;         ;;erc-youtube
  ;;         expand-region
  ;;         find-things-fast
  ;;         flycheck
  ;;         flymake-cursor
  ;;         fold-dwim
  ;;         fold-dwim-org
  ;;         fuzzy
  ;;         gh
  ;;         gist
  ;;         git-link
  ;;         haml-mode
  ;;         helm
  ;;         highlight-parentheses
  ;;         htmlize
  ;;         idle-highlight
  ;;         ido-ubiquitous
  ;;         ido-vertical-mode
  ;;         inf-ruby
  ;;         js2-mode
  ;;         link
  ;;         logito
  ;;         magit
  ;;         markdown-mode
  ;;         markup-faces
  ;;         maxframe
  ;;         org
  ;;         paredit
  ;;         pcache
  ;;         pkg-info
  ;;         popup
  ;;         powerline
  ;;         rainbow-delimiters
  ;;         s
  ;;         sass-mode
  ;;         smex
  ;;         ;; swoop
  ;;         yaml-mode
  ;;         yasnippet))
  )
