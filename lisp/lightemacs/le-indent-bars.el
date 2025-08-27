;;; le-indent-bars.el --- le-indent-bars -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The indent-bars package enhances code readability by providing visual
;; indentation guides, optimized for speed and customization.
;; (Useful for Yaml or Python files.)
;;
;; It supports both space and tab-based indentation and offers optional
;; tree-sitter integration, which includes features like scope focus. The
;; appearance of the guide bars is highly customizable, allowing you to adjust
;; their color, blending, width, position, and even apply a zigzag pattern.
;;
;; Depth-based coloring with a customizable cyclical palette adds clarity to
;; nested structures. The package also features fast current-depth highlighting,
;; configurable bar changes, and the ability to display bars on blank lines.
;; Additionally, it maintains consistent bar depth within multi-line strings and
;; lists, and it works seamlessly in terminal environments using a vertical bar
;; character.
;;
;; URL: https://github.com/jdtsmith/indent-bars

;;; Code:

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  indent-bars
  :commands indent-bars-mode
  :init
  ;; Setting this to nil is not reliable
  ;; https://github.com/jdtsmith/indent-bars?tab=readme-ov-file#stipples
  (setq indent-bars-prefer-character t)

  ;; When `indent-bars-prefer-character' is set to t, displaying indent bars on
  ;; blank lines causes cursor movement issues when moving downward, resulting
  ;; in abrupt shifts of the window start or cursor position.
  (setq indent-bars-display-on-blank-lines nil)

  ;; Define the global variable `lightemacs-indent-bars-mode-hook-list'
  (lightemacs-define-mode-hook-list indent-bars-mode
                                    nil))

(provide 'le-indent-bars)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-indent-bars.el ends here
