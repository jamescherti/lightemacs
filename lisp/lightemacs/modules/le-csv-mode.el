;;; le-csv-mode.el --- le-csv-mode -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `csv-mode' is a major mode for Emacs that transforms the experience of
;; editing Comma-Separated Value files by bridging the gap between raw text
;; manipulation and a spreadsheet interface.
;;
;; Its most impactful feature is the ability to align fields into visually
;; distinct, vertically synchronized columns, which instantly renders dense,
;; delimited text into a readable table format.
;;
;; Beyond mere visualization, the package provides tools for structural editing,
;; allowing users to sort rows based on specific fields, transpose columns, and
;; perform rectangular operations such as killing or yanking specific vertical
;; slices of data.
;;
;; URL: https://elpa.gnu.org/packages/csv-mode.html

;;; Code:

(eval-and-compile
  (require 'le-core-package-manager))

(lightemacs-use-package csv-mode
  :commands (csv-mode
             csv-align-mode)
  :hook (csv-mode . lightemacs-csv-mode--setup)

  :preface
  (defun lightemacs-csv-mode--setup ()
    "Setup `csv-mode'."
    (csv-guess-set-separator)
    (csv-align-mode))

  :mode
  ("\\.[cC][sS][vV]\\'" . csv-mode)

  :custom
  (csv-align-max-width 100)
  (csv-separators '("," "\t" ";" "|")))

(provide 'le-csv-mode)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-csv-mode.el ends here
