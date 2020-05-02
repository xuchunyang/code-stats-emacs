# [Code::Stats](https://codestats.net/) plugin for Emacs
[![Melpa](https://melpa.org/packages/code-stats-badge.svg)](https://melpa.org/#/code-stats)

## Setup

Sample configuration:

``` emacs-lisp
(setq code-stats-token "your-token")
(add-hook 'prog-mode-hook #'code-stats-mode)
(run-with-idle-timer 30 t #'code-stats-sync)
(add-hook 'kill-emacs-hook (lambda () (code-stats-sync :wait)))
```

- The variable `code-stats-token` should store your API token. You should keep
  the token privately, an idea is using auth-source, put your API token to
  `~/.authinfo[.gpg]`, e.g.,

        machine codestats.net password your-token

  then retrieve it:

        ```emacs-lisp
        (setq code-stats-token
          (auth-source-pick-first-password :host "codestats.net"))
        ```

- The buffer-local minor mode `code-stats-token` tracks codestats for current
  buffer.
- the function `code-stats-sync` syncs codestats to https://codestats.net/

## Requires

- Emacs 25.1
- Package [request](https://github.com/tkf/emacs-request)
