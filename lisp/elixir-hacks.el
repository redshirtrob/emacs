;;; elixir-hacks --- Elixir editing

;;; Commentary:
;;; Copy the syntax checker from here:
;;;   https://github.com/ananthakumaran/dotfiles/blob/master/.emacs.d/init-elixir.el

;;; Code:

(require 'flycheck)
(require 'elixir-mode)
(require 'alchemist)

(flycheck-define-checker elixir-mix
    "An Elixir syntax checker using the Elixir interpreter.
See URL `http://elixir-lang.org/'."
    :command ("mix"
              "compile"
              source)
    :error-patterns
    ((error line-start "** (" (zero-or-more not-newline) ") "
            (zero-or-more not-newline) ":" line ": " (message) line-end)
     (warning line-start
              (one-or-more (not (syntax whitespace))) ":"
              line ": "
              (message)
              line-end))
    :modes elixir-mode)

;; (add-to-list 'flycheck-checkers 'elixir-mix)

;; (add-hook 'elixir-mode-hook 'flycheck-mode)
(add-hook 'elixir-mode-hook 'alchemist-mode)
(add-hook 'elixir-mode-hook 'company-mode)

(provide 'elixir-hacks)
;;; elixir-hacks.el ends here
