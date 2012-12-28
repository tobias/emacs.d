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
      '(clojure-mode
        coffee-mode
        erc-hl-nicks
        find-things-fast
        flymake-cursor
        fold-dwim
        fold-dwim-org
        gh
        gist
        haml-mode
        highlight-parentheses
        htmlize
        idle-highlight
        ido-ubiquitous
        magit
        markdown-mode
        maxframe
        midje-mode
        nrepl
        paredit
        sass-mode
        textile-mode
        todochiku
        yaml-mode
        zenburn-theme
        ruby-electric)))
