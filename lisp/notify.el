(require 'notifications)

(defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")

(defun tc/notify (title message)
  (if tc/macos-p
      (start-process "terminal-notifier"
                     "*terminal-notifier*"
                     terminal-notifier-command
                     "-message" message
                     "-sound" "Purr"
                     "-title" (concat "\"" title "\"")
                     "-activate" "org.gnu.Emacs")
      (progn
        (notifications-notify
         :title title
         :body message)
        (start-process-shell-command "alert-sound" nil
                                     "ogg123 /usr/share/sounds/freedesktop/stereo/bell.oga"))))
