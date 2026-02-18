;;; le-outline-indent.el --- le-outline-indent -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The outline-indent Emacs package provides a minor mode that enables code
;; folding based on indentation levels.
;;
;; In addition to code folding, *outline-indent* allows:
;; - Moving indented blocks up and down
;; - Indenting/unindenting to adjust indentation levels
;; - Inserting a new line with the same indentation level as the current line
;; - Move backward/forward to the indentation level of the current line
;; - and other features.
;;
;; This mode is enabled by default for YAML and Python files.

;;; Code:

(require 'lightemacs-module)

(lightemacs-module-package outline-indent
  :commands (outline-indent-minor-mode
             outline-indent-shift-left
             outline-indent-shift-right
             outline-indent-move-subtree-up
             outline-indent-move-subtree-down
             outline-indent-forward-same-level
             outline-indent-backward-same-level
             outline-indent-select
             outline-indent-insert-heading
             outline-indent-toggle-fold
             outline-indent-open-fold-rec
             outline-indent-close-fold
             outline-indent-open-fold
             outline-indent-open-folds
             outline-indent-close-folds)
  :init
  (setq outline-indent-ellipsis lightemacs-ellipsis))

(provide 'le-outline-indent)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-outline-indent.el ends here
