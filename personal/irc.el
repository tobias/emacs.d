(require 'erc)
(require 'todochiku)
(require 'erc-highlight-nicknames)
(require 'erc-nicklist)

(setq erc-nicklist-use-icons nil)
(setq erc-fill-column 98)

;;; change header line face if disconnected
(defface erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
          (t 'erc-header-line-disconnected))))

(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

;; display # of members in mode line
(define-minor-mode ncm-mode "" nil
  (:eval
   (let ((ops 0)
         (voices 0)
         (members 0))
     (maphash (lambda (key value)
                (when (erc-channel-user-op-p key)
                  (setq ops (1+ ops)))
                (when (erc-channel-user-voice-p key)
                  (setq voices (1+ voices)))
                (setq members (1+ members)))
              erc-channel-users)
     ;; (format " %S/%S/%S" ops voices members)
     (format ":%S members" members)
     )))

(add-hook 'erc-mode-hook 'ncm-mode)

(defun irc-connect-internal ()
  (interactive)
  (erc :server my-internal-irc-server :port 6667 :nick "tcrawley" :full-name "Toby Crawley"))
(defun irc-connect-freenode ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "tcrawley" :password my-freenode-password :full-name "Toby Crawley"))
(defun irc-connect-all ()
  (interactive)
  (irc-connect-internal)
  (irc-connect-freenode))

(defvar irc-channels-for-alerting
  '()
  "IRC channels to watch for alerting.")

(defun irc-growl (channel message)
  "Displays an irc message to growl/libnotify via todochiku.
Notice will be sticky if the message is a query."
  (let ((split-message (irc-split-nick-and-message message)))
    (if split-message
        (todochiku-message
         (concat "<" (nth 0 split-message) "> on " channel)
         (escape-html (nth 1 split-message))
         ""
         (not (string-match "^#" channel)) ;; be sticky if it's a query
         ))))

(defun irc-split-nick-and-message (msg)
  "Splits an irc message into nick and the rest of the message.
Assumes message is either of two forms: '* nick does something' or '<nick> says something'"
  (if (string-match "^[<\\*] ?\\(.*?\\)>? \\(.*\\)$" msg)
      (cons (match-string 1 msg)
            (cons (match-string 2 msg)
                  ()))
    ()))

(defun irc-alert-on-message (channel msg)
  "Plays a sound and growl notifies a message."
  (and (string-match "^[*<][^*]" msg)
       (> (length msg) 0)
       (or (member channel irc-channels-for-alerting)
           (not (string-match "^#" channel)) ;;query
           (string-match (erc-current-nick) msg))
       (progn
         (play-irc-alert-sound)
         (irc-growl channel msg))))

(defun irc-alert ()
  (save-excursion
    (irc-alert-on-message (buffer-name) (buffer-substring (point-min) (point-max)))))

(add-hook 'erc-insert-post-hook 'irc-alert)

(defun play-irc-alert-sound ()
  (start-process-shell-command "alert-sound" nil
                               (if (eq system-type 'darwin)
                                   "say -v Zarvox -r 500 heyy"
                                 "mplayer /usr/share/sounds/purple/alert.wav")))

(defun escape-html (str)
  "Escapes [<>&\n] from a string with html escape codes."
  (and str
       (replace-regexp-in-string "<" "&lt;"
         (replace-regexp-in-string ">" "&gt;"
           (replace-regexp-in-string "&" "&amp;"
             (replace-regexp-in-string "\n" "" str))))))

(define-key erc-mode-map (kbd "C-c q")
  (lambda (nick)
    (interactive (list (completing-read "Query nick: " erc-channel-users)))
    (erc-cmd-QUERY nick)))

(define-key erc-mode-map (kbd "C-c y") `yank-to-gist)

(setq erc-prompt
      (lambda ()
        (if (and (boundp 'erc-default-recipients) (erc-default-target))
            (erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
          (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t))))


;; (require 'erc-summarize)
;; (erc-summarize-add-hooks)
