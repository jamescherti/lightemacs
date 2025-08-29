;;; le-flavor-big.el --- Group: Default modules -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The "big" flavor enables all modules except:
;;   - le-treesit-auto (optional, as not all users require Treesit)
;;   - le-easysession (optional, for users who want session management)
;;
;; Command to generate this flavor:
;; ls le-*.el -1 | grep -v "^le-core" | sed 's/\.el$//' | \
;;   grep -v ^le-flavor- | grep -v ^le-treesit-auto | grep -v ^le-easysession

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-load-modules
 '(;; All modules:
   le-flymake
   le-ace-window
   le-aggressive-indent
   le-apheleia
   le-autorevert
   le-avy
   le-bufferfile
   le-cape
   le-compile-angel
   le-consult-dir
   le-consult
   le-corfu
   le-corfu-prescient
   le-default-settings
   le-diff-hl
   le-diminish
   le-dired
   le-dired-filter
   le-display-line-numbers
   le-dtrt-indent
   le-dumb-jump
   le-easy-escape
   le-elec-pair
   le-elisp-refs
   le-embark-consult
   le-embark
   le-enhanced-evil-paredit
   le-evil-collection
   le-evil-commentary
   le-evil
   le-evil-snipe
   le-evil-surround
   le-expand-region
   le-gcmh
   le-git-modes
   le-goto-chg
   le-group-emacs-lisp
   le-group-evil
   le-group-yaml
   le-helpful
   le-highlight-defined
   le-indent-bars
   le-keybindings
   le-magit
   le-marginalia
   le-markdown-mode
   le-markdown-toc
   le-orderless
   le-org-appear
   le-org
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
   le-theme
   le-undo-fu
   le-undo-fu-session
   le-vertico
   le-vertico-prescient
   le-vim-tab-bar
   le-vterm
   le-wgrep
   le-which-key
   le-winner
   le-yaml-mode
   le-yaml-ts-mode
   le-yasnippet
   le-yasnippet-snippets
   ))

;;; Provide

(provide 'le-flavor-big)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-flavor-big.el ends here
