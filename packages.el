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
          cl-lib
          clojure-mode
          clojure-test-mode
          coffee-mode
          dash
          diminish
          dired-details
          erc-hl-nicks
          erc-image
          erc-tweet
          erc-youtube
          find-things-fast
          flymake-cursor
          fold-dwim
          fold-dwim-org
          fuzzy
          gh
          gist
          haml-mode
          helm
          highlight-parentheses
          htmlize
          idle-highlight
          ido-ubiquitous
          ido-vertical-mode
          inf-ruby
          logito
          magit
          markdown-mode
          markup-faces
          maxframe
          org
          paredit
          pcache
          pkg-info
          popup
          powerline
          ruby-electric
          s
          sass-mode
          scpaste
          swoop
          textile-mode
          todochiku
          yaml-mode
          yasnippet)))
