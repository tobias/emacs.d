;; for el-get

(setq el-get-sources
      '(el-get
        todochiku
        ;; maxframe                        
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
        (:name gist
               :type git
               :url "https://github.com/mcfunley/gist.el.git"
               :features gist)
        (:name highlight-parentheses
               :type git
               :url "git://github.com/nschum/highlight-parentheses.el.git"
               :features highlight-parentheses)
	(:name idle-highlight 
	       :type git
	       :url "https://github.com/emacsmirror/idle-highlight.git")))

