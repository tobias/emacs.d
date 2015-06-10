(when (string-prefix-p "24" emacs-version)
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

  (package-initialize)
  ;;(package-refresh-contents)

  (mapc (lambda (pkg)
          (and (not (package-installed-p pkg))
               (package-install pkg)))
        '(ace-window
          adoc-mode
          align-cljlet
          auto-complete
          cider
          cl-lib
          clj-refactor
          clojure-mode
          coffee-mode
          dash
          diminish
          dired-details
          ;; downplay-mode
          erc-hl-nicks
          ;;erc-image
          ;;erc-tweet
          ;;erc-youtube
          expand-region
          find-things-fast
          flymake-cursor
          fold-dwim
          fold-dwim-org
          fuzzy
          gh
          gist
          git-link
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
          rainbow-delimiters
          s
          sass-mode
          smex
          ;; swoop
          yaml-mode
          yasnippet)))
