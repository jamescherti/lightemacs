;;; le-treesit-fold.el --- le-treesit-fold -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `treesit-fold' is an Emacs package that provides intelligent code folding by
;; leveraging the structural understanding of the built-in tree-sitter parser
;; (available in Emacs 29+). Unlike traditional folding methods that rely on
;; regular expressions or indentation, treesit-fold uses the actual syntax tree
;; of the code to accurately identify foldable regions such as functions,
;; classes, comments, and documentation strings. This allows for faster and more
;; precise folding behavior that respects the grammar of the programming
;; language, ensuring that fold boundaries are always syntactically correct even
;; in complex or nested code structures.
;;
;; https://github.com/emacs-tree-sitter/treesit-fold

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package treesit-fold
  :commands (treesit-fold-close
             treesit-fold-close-all
             treesit-fold-open
             treesit-fold-close
             treesit-fold-toggle
             treesit-fold-open-all
             treesit-fold-mode
             global-treesit-fold-mode
             treesit-fold-open-recursively
             treesit-fold-line-comment-mode)
  :init
  (setq treesit-fold-line-count-show t)
  (setq treesit-fold-line-count-format lightemacs-ellipsis)
  ;; (setq truncate-string-ellipsis lightemacs-ellipsis)
  :config
  (set-face-attribute 'treesit-fold-replacement-face nil
                      :foreground "#808080"
                      :box nil
                      :weight 'bold))

(provide 'le-treesit-fold)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-treesit-fold.el ends here
