  (when (file-exists-p "~/work/backend/elisp")
    (add-to-list 'load-path "~/work/backend/elisp")
    (require 'clubhouse-backend)

    (setq clubhouse-backend-directory "~/work/backend")

    )

(require 'cider)

(setq tc/ch-repl-prod-connection
      "(do
  (require '[datomic.api :as d])
  (def PROD-C (d/connect \"datomic:ddb://us-east-1/clubhouse-production-datomic-20180202/clubhouse\"))
  (def PROD-DB (d/db PROD-C)))")

(setq tc/ch-repl-staging-connection
      "(do
  (require '[datomic.api :as d])
  (def STAGING-C (d/connect \"datomic:ddb://us-east-1/clubhouse-staging-datomic/clubhouse\"))
  (def STAGING-DB (d/db STAGING-C)))")

(setq tc/ch-repl-completions
      `((prod-db . ,tc/ch-repl-prod-connection)
        (staging-db . ,tc/ch-repl-staging-connection)))

(defun tc/ch-repl ()
  "Insert Toby's bootstapping code into the REPL. (hat tip to Daniel Gregoire)"
  (interactive)
  (cider-ensure-connected)
  (let* ((default 'prod-db)
         (choice (completing-read (format "REPL purpose (%s): " default)
                                  tc/ch-repl-completions nil t nil nil default))
         (inhibit-read-only t))
    (save-excursion
      (goto-char cider-repl-input-start-mark)
      (insert (alist-get (intern choice) tc/ch-repl-completions)))))

(cider-repl-add-shortcut "ch-repl" #'tc/ch-repl)
