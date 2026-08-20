;;; le-flavor-big.el --- Group: Default modules -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The "big" flavor enables is a good flavor for software developers.

;;; Code:

(require 'lightemacs-module)

(lightemacs-module-load
 '(;; All modules:
   le-aggressive-indent
   le-apheleia
   le-autorevert
   le-avy
   le-buffer-terminator
   le-bufferfile
   le-cape
   le-compile-angel
   le-consult
   le-corfu
   le-corfu-prescient
   le-csv-mode
   le-default-keybindings
   le-default-settings
   le-diff-hl
   le-dired
   le-dired-filter
   le-display-line-numbers
   le-dtrt-indent
   le-dumb-jump
   le-easy-escape
   le-elec-pair
   le-elisp-autofmt
   le-elisp-refs
   le-embark
   le-embark-consult
   le-expand-region
   le-flymake
   le-git-modes
   le-goto-chg
   le-helpful
   le-highlight-defined
   le-indent-bars
   le-kirigami
   le-magit
   le-marginalia
   le-maybe-markdown-ts
   le-maybe-yaml-ts
   le-orderless
   le-org
   le-org-appear
   le-outline
   le-outline-indent
   le-page-break-lines
   le-paredit
   le-paren
   le-persist-text-scale
   le-prescient
   le-recentf
   le-savehist
   le-saveplace
   le-stripspace
   le-term
   le-theme
   le-undo-fu
   le-undo-fu-session
   le-vertico
   le-vertico-prescient
   le-vim-tab-bar
   le-which-key
   le-winner
   le-yasnippet
   le-yasnippet-snippets))

;;; Provide

(provide 'le-flavor-big)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flavor-big.el ends here
