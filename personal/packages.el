;; for el-get

(setq el-get-sources
      '(el-get
        todochiku
        maxframe
        magit
        ;;js2-mode
        markdown-mode
        textile-mode
        yaml-mode
        (:name ruby-electric
               :type http
               :url "http://shylock.uw.hu/Emacs/ruby-electric.el")
        rvm
        ;;inf-ruby
        yasnippet
        (:name yasnippets-rails
               :type git
               :url "https://github.com/eschulte/yasnippets-rails.git")
        elunit                          ;rinari needs this
        (:name rinari
               :type git
               :url "http://github.com/eschulte/rinari.git"
               :load-path ("." "util" "util/jump")
               :compile "nothing" ;("\.el$" "util")
               :features rinari)
        haml-mode
        sass-mode
        (:name full-ack
               :type git
               :url "https://github.com/nschum/full-ack.git")
        color-theme
        (:name color-theme-arjen
               :type git
               :url "https://github.com/credmp/color-theme-arjen.git")
        (:name gist
               :type git
               :url "https://github.com/mcfunley/gist.el.git"
               :features gist)
        ;; idle-highlight fails to load from elpa for some reason
        (:name idle-highlight
               :type http
               :url "https://github.com/technomancy/dotfiles/raw/master/.emacs.old/idle-highlight.el")))
