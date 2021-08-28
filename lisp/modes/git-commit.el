(when shortcut-elisp-loaded
  (define-key git-commit-mode-map (kbd "C-c l") 'shortcut-backend-insert-shortcut-story-url)
  (define-key git-commit-mode-map (kbd "C-c C-l") 'shortcut-backend-insert-shortcut-story-url)

  (define-key git-commit-mode-map (kbd "C-c a") 'shortcut-backend-insert-co-authored-by)
  (define-key git-commit-mode-map (kbd "C-c C-a") 'shortcut-backend-insert-co-authored-by)

  )
