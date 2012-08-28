;; for el-get

(setq el-get-sources
      '((:name ruby-electric
               :type http
               :url "http://shylock.uw.hu/Emacs/ruby-electric.el")
        (:name rinari
               :type git
               :url "http://github.com/eschulte/rinari.git"
               :load-path ("." "util" "util/jump")
               :compile "nothing" ;("\.el$" "util")
               :features rinari)
        (:name full-ack
               :type git
               :url "https://github.com/nschum/full-ack.git")
        ;; (:name gist
        ;;        :type git
        ;;        :url "https://github.com/defunkt/gist.el.git"
        ;;        :features gist)
        (:name highlight-parentheses
               :type git
               :url "git://github.com/nschum/highlight-parentheses.el.git"
               :features highlight-parentheses)
	(:name idle-highlight 
	       :type git
	       :url "https://github.com/emacsmirror/idle-highlight.git")
	;; (:name slime
	;;        :description "Superior Lisp Interaction Mode for Emacs"
	;;        :type git
	;;        :module "slime"
	;;        :url "https://github.com/nablaone/slime.git"
	;;        :load-path ("." "contrib")
	;;        :compile (".")
	;;        )
        )) 

(setq my-packages 
      (append 
       '(todochiku markdown-mode textile-mode yaml-mode
	 clojure-mode midje-mode paredit rvm elunit haml-mode sass-mode color-theme)
       (mapcar 'el-get-source-name el-get-sources)))

;; ELPA

(when (string-prefix-p "24" emacs-version)
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))
