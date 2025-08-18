;;; lem-outline-indent.el --- lem-outline-indent -*- lexical-binding: t -*-

;; Author: James Cherti
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

(use-package outline-indent
  :diminish outline-indent-minor-mode
  :commands (outline-indent-minor-mode
             outline-indent-insert-heading)
  :init
  (setq outline-indent-ellipsis lightemacs-ellipsis))

(provide 'lem-outline-indent)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-outline-indent.el ends here
