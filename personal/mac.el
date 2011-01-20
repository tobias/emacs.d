(when (eq system-type 'darwin)
  ;; emacs 23 breaks the command -> meta mapping. This fixes it.
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil)

  (setq-default ispell-program-name "aspell")

  ;; sets the cursor to a bar. useful on carbon emacs 23, since a block
  ;; cursor obscures the character below
  (setq initial-frame-alist
        (cons '(cursor-type . bar)
              (copy-alist initial-frame-alist)))

  (setq default-frame-alist
        (cons '(cursor-type . bar)
              (copy-alist default-frame-alist))))


