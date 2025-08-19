;;; le-apheleia.el --- le-apheleia -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Apheleia is a package that runs code formatters asynchronously without
;; disrupting the cursor position. Code formatters like Shfmt, Black and
;; Prettier ensure consistency and improve collaboration by automating
;; formatting, but running them on save can introduce latency (e.g., Black takes
;; around 200ms on an empty file) and unpredictably move the cursor when
;; modifying nearby text.
;;
;; Apheleia solves both problems across all languages, replacing
;; language-specific packages like Blacken and prettier-js. It does this by
;; invoking formatters in an `after-save-hook`, ensuring changes are applied
;; only if the buffer remains unmodified.
;;
;; To maintain cursor stability, Apheleia generates an RCS patch, applies it
;; selectively, and employs a dynamic programming algorithm to reposition the
;; cursor if necessary. If the formatting alters the vertical position of the
;; cursor in the window, Apheleia adjusts the scroll position to preserve visual
;; continuity across all displayed instances of the buffer. This allows enjoying
;; automated code formatting without sacrificing editor responsiveness or
;; usability.
;;
;; URL: https://github.com/radian-software/apheleia

;;; Code:

(use-package apheleia
  :commands (apheleia-mode
             apheleia-global-mode))

(provide 'le-apheleia)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-apheleia.el ends here
