(when (not tc/presentation-mode-p)
  (add-hook 'text-mode-hook 'turn-on-flyspell))
