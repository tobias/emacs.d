(defun tc/insert-clubhouse-story-url ()
  "Looks forward in the buffer for a story branch, and uses the
story id to generate and insert a url to the story."
  (interactive)
  (insert
   (save-excursion
     (when (re-search-forward "branch .*/ch\\([0-9]+\\)/")
       (format "https://app.clubhouse.io/internal/story/%s/" (match-string 1))))))

(define-key git-commit-mode-map (kbd "C-c l") 'tc/insert-clubhouse-story-url)
(define-key git-commit-mode-map (kbd "C-c C-l") 'tc/insert-clubhouse-story-url)
