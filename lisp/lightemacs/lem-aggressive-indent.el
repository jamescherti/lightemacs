;;; lem-aggressive-indent.el --- lem-aggressive-indent -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `electric-indent-mode' is sufficient to maintain proper code alignment during
;; straightforward typing. However, when you begin shifting code blocks,
;; transposing lines, or manipulating s-expressions, indentation inconsistencies
;; are likely to occur.
;;
;; `aggressive-indent-mode' is a minor mode that ensures code remains
;; consistently indented. It automatically reindents after every modification,
;; providing greater reliability than `electric-indent-mode'.

;;; Code:

(use-package aggressive-indent
  :commands aggressive-indent-mode)

(provide 'lem-aggressive-indent)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-aggressive-indent.el ends here
