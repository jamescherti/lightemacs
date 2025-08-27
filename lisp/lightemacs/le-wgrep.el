;;; le-wgrep.el --- le-wgrep -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The wgrep (Writable Grep) package enables you to convert a grep,
;; consult-ripgrep, or Embark Export buffers into an editable interface. It
;; allows in-place modification of matched lines within the results buffer,
;; which can then be propagated back to the corresponding files upon
;; confirmation. This facilitates precise, bulk edits across multiple files
;; efficiently, eliminating the need to open each file individually, and
;; effectively transforms the grep results buffer into a controlled, multi-file
;; editing environment.
;;
;; URL: https://github.com/mhayashi1120/Emacs-wgrep

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(eval-and-compile
  (require 'use-package))

(lightemacs-use-package
  wgrep
  :commands (wgrep-change-to-wgrep-mode
             wgrep-finish-edit))

(provide 'le-wgrep)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-wgrep.el ends here
