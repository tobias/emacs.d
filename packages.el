(when (string-prefix-p "24" emacs-version)
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (package-initialize)

  (mapc (lambda (pkg)
          (and (not (package-installed-p pkg))
               (package-install pkg)))
        '(ace-window
          adoc-mode
          auto-complete
          cider
          clojure-test-mode
          coffee-mode
          diminish
          dired-details
          erc-hl-nicks
          erc-image
          erc-tweet
          erc-youtube
          find-things-fast
          flymake-cursor
          fold-dwim-org
          fold-dwim
          fuzzy
          gh
          gist
          helm
          highlight-parentheses
          htmlize
          idle-highlight
          ido-ubiquitous
          ido-vertical-mode
          logito
          magit
          markdown-mode
          markup-faces
          maxframe
          ;;midje-mode
          cl-lib
          clojure-mode
          ;; Running from a checkout currently
          ;;ox-reveal
          org
          paredit
          pcache
          pkg-info
          dash
          popup
          ruby-electric
          s
          sass-mode
          haml-mode
          scpaste
          swoop
          textile-mode
          todochiku
          yaml-mode
          yasnippet)))
