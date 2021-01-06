(when clubhouse-backend-directory
  (define-key git-commit-mode-map (kbd "C-c l") 'clubhouse-backend-insert-clubhouse-story-url)
  (define-key git-commit-mode-map (kbd "C-c C-l") 'clubhouse-backend-insert-clubhouse-story-url)

  (define-key git-commit-mode-map (kbd "C-c a") 'clubhouse-backend-insert-co-authored-by)
  (define-key git-commit-mode-map (kbd "C-c C-a") 'clubhouse-backend-insert-co-authored-by)

  )
