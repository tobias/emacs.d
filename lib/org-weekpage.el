;; org-weekpage.el --- Org-Mode Week Page.
;;

;;; License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in he hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Installation
;;
;; Add org-weekpage.el to your load path and add (require 'org-weekpage)
;; to your .emacs.

;;; Configuration:
;;
;; No keys are defined by default. So you may wish to add something
;; like the following to your .emacs as well:
;;
;; (define-key weekpage-mode-map (kbd "<C-left>") 'weekpage-prev)
;; (define-key weekpage-mode-map (kbd "<C-right>") 'weekpage-next)
;; (define-key weekpage-mode-map "\C-c." 'weekpage-time-stamp)
;;
;; (global-set-key [f11] 'this-weeks-weekpage)
;; (global-set-key [f10] 'last-weeks-weekpage)
;; (global-set-key "\C-con" 'this-weeks-weekpage)
;; (global-set-key "\C-coN" 'find-weekpage)

(eval-when-compile (require 'cl))

(setq weekpage-path "~/notes/weeks/")

(defvar weekpage-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "The key map for weekpage buffers.")

(setq weekpage-template-file (expand-file-name "~/.weekpage-template.org"))

(defun start-of-week (date)
  (destructuring-bind (seconds minutes hour day month year dow dst offset)
      (decode-time date)
    (encode-time seconds minutes hour (+ 1 (- day dow)) month year)))

(defun find-weekpage (&optional date)
  "Go to the week page for the specified date, or this week's if none is specified."
  (interactive (list
                (org-read-date "" 'totime nil nil
                               (current-time) "")))
  (let ((date (start-of-week (or date (current-time)))))
    (find-file (expand-file-name (concat weekpage-path (format-time-string "%Y-%m-%d" date) ".org"))))
  (when (and (eq 0 (buffer-size))
             (file-exists-p weekpage-template-file))
    ;; Insert an initial template for the page
    (insert-file-contents weekpage-template-file)
    (save-buffer)))

(defun weekpage-p ()
  "Return true if the current buffer is visiting a weekpage"
  (if (weekpage-date)
      t
    nil))

(defun weekpage-date ()
  "Return the date for the weekpage visited by the current buffer
or nil if the current buffer isn't visiting a weekpage"
  (let ((file (buffer-file-name))
        (root-path (expand-file-name weekpage-path)))
    (if (and file
               (string= root-path (substring file 0 (length root-path)))
               (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\).org$" file))
        (flet ((d (i) (string-to-number (match-string i file))))
          (encode-time 0 0 0 (d 3) (d 2) (d 1)))
      nil)))


(defun maybe-weekpage ()
  "Set up weekpage stuff if the org file being visited is in the weekpage folder"
  (let ((date (weekpage-date)))
    (when date
      ; set up the weekpage key map
      (use-local-map weekpage-mode-map)
      (set-keymap-parent weekpage-mode-map
                         org-mode-map)
      (run-hooks 'weekpage-hook))))

(add-hook 'org-mode-hook 'maybe-weekpage)

(defun weekpage-next ()
  (interactive)
  (find-weekpage
   (seconds-to-time (+ (time-to-seconds (weekpage-date))
                       (* 7 86400))))
  (run-hooks 'weekpage-movement-hook))

(defun weekpage-prev ()
  (interactive)
  (find-weekpage
   (seconds-to-time (- (time-to-seconds (weekpage-date))
                       (* 7 86400))))
  (run-hooks 'weekpage-movement-hook))

(defun this-weeks-weekpage ()
  "Go straight to today's week page without prompting for a date."
  (interactive)
  (find-weekpage)
  (run-hooks 'weekpage-movement-hook))

(defun last-weeks-weekpage ()
  "Go straight to last week's page without prompting for a date."
  (interactive)
  (find-weekpage
   (seconds-to-time (- (time-to-seconds (current-time))
                      (* 7 86400))))
  (run-hooks 'weekpage-movement-hook))

(defun weekpage-time-stamp ()
  "Works like (and is basically a thin wrapper round)
org-time-stamp except the default date will be the date of the weekpage."
  (interactive)
  (unless (org-at-timestamp-p)
    (insert "<" (format-time-string "%Y-%m-%d %a" (weekpage-date)) ">")
    (backward-char 1))
  (org-time-stamp nil))

(defun weekpage-new-item ()
  "Switches to the current weekpage and inserts a top level heading and a timestamp"
  (interactive)
  (this-weeks-weekpage)
  (end-of-buffer)
  (if (not (bolp))
      (insert "\n"))
  (insert "* <" (format-time-string "%Y-%m-%d %a" (weekpage-date)) "> "))


(provide 'org-weekpage)
