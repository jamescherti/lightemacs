;;; le-flavor-big.el --- Group: Default modules -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The "big" flavor enables all modules except:
;;   - le-treesit-auto (optional, as not all users require Treesit)
;;   - le-easysession (optional, for users who want session management)
;;   - le-vterm (optional and requires external dependencies)
;;   - le-gcmh (optional, for users who want gcmh)
;;   - le-compile-angel (optional, for users who want to compile all .el files)
;;
;; Command to generate this flavor:
;; ls le-*.el -1 | grep -v "^le-core" | sed 's/\.el$//' | \
;;   grep -v '^le-flavor-' | grep -v '^le-treesit-auto' \
;;   | grep -v '^le-easysession' | grep -v '-evil' | grep -v '^le-vterm' \
;;   | grep -v '^le-gcmh' | | grep -v '^le-compile-angel' \
;;   | grep -v '^le-yaml' | grep -v '^le-group-emacs-lisp' | \
;;   grep -v '^le-group-markdown' | grep -v '^le-diminish' | sort

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-load-modules
 '(;; All modules:
   le-theme
   le-ace-window
   le-aggressive-indent
   le-apheleia
   le-autorevert
   le-avy
   le-bufferfile
   le-cape
   le-consult
   le-consult-dir
   le-corfu
   le-corfu-prescient
   le-default-settings
   le-diff-hl
   le-dired
   le-dired-filter
   le-display-line-numbers
   le-dtrt-indent
   le-dumb-jump
   le-easy-escape
   le-elec-pair
   le-elisp-refs
   le-embark
   le-embark-consult
   le-expand-region
   le-flymake
   le-git-modes
   le-goto-chg
   le-group-yaml
   le-helpful
   le-highlight-defined
   le-indent-bars
   le-keybindings
   le-magit
   le-marginalia
   le-markdown-mode
   le-markdown-toc
   le-edit-indirect
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
   le-undo-fu
   le-undo-fu-session
   le-vertico
   le-vertico-prescient
   le-vim-tab-bar
   le-wgrep
   le-which-key
   le-winner
   le-yasnippet
   le-yasnippet-snippets
   le-csv-mode
   ))

;;; Provide

(provide 'le-flavor-big)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flavor-big.el ends here
