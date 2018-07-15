# [Code::Stats](https://codestats.net/) plugin for Emacs

## Setup

``` emacs-lisp
(setq code-stats-token "your-token")
(add-hook 'prog-mode-hook #'code-stats-mode)
(run-with-idle-timer 30 t #'code-stats-sync)
(add-hook 'kill-emacs-hook (lambda () (code-stats-sync :wait)))
```
