;;; le-apheleia.el --- le-apheleia -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(eval-and-compile
  (require 'lightemacs))

(defvar lightemacs-apheleia-fix-screen-lines-bug t
  "If non-nil, apply a fix for Apheleia window start issues in folded buffers.")

(lightemacs-use-package apheleia
  :commands (apheleia-mode
             apheleia-global-mode)
  :preface
  ;; TODO: Send patch to Apheleia
  (defun lightemacs--apheleia-use-screen-lines (orig-fun &rest args)
    "Use visual lines instead of logical lines for scroll calculation.

In `apheleia--apply-rcs-patch', the vertical position of the point relative to
the window start is calculated using `count-lines'. This function counts logical
buffer lines (newlines), including those hidden by folds or invisible overlays.

However, Apheleia attempts to restore this position using `scroll-down', which
operates on visual screen lines.

When code folding is active (e.g., 50 lines collapsed into 1), `count-lines'
returns the logical count (50), while the visual distance is only 1. Apheleia
then instructs the window to scroll down by 50 lines, causing the viewport to
jump significantly.

This advice temporarily swaps `count-lines' with `count-screen-lines' during the
patch application. `count-screen-lines' respects text visibility, returning the
correct visual distance. This ensures the unit of measurement used for
calculation matches the unit used for scrolling, preserving the user's view.

ORIG-FUN and ARGS are the original function and its arguments."
    ;; TODO: Send patch to Apheleia
    (lightemacs-save-window-start
      (apply orig-fun args))
    ;; (cl-letf (((symbol-function 'count-lines) #'count-screen-lines))
    ;;   (apply orig-fun args))
    )

  :config
  (when lightemacs-apheleia-fix-screen-lines-bug
    (advice-add 'apheleia--apply-rcs-patch :around
                #'lightemacs--apheleia-use-screen-lines)))

(provide 'le-apheleia)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-apheleia.el ends here
