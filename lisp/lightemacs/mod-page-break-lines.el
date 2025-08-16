;;; mod-page-break-lines.el --- mod-page-break-lines -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The `page-break-lines' minor mode in Emacs visually replaces ASCII form-feed
;; characters (typically `^L`) with horizontal line separators in buffers,
;; making page breaks easier to see without affecting the actual text.
;;
;; When enabled, it renders each form-feed as a clean horizontal line spanning
;; the window width, providing a clear visual cue for logical divisions, such as
;; chapters or sections, in source code, text files, or documents, while
;; preserving the underlying file content for editing or printing.
;;
;; URL: https://github.com/purcell/page-break-lines

;;; Code:

(use-package page-break-lines
  :commands (page-break-lines-mode
             global-page-break-lines-mode))

(provide 'mod-page-break-lines)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-page-break-lines.el ends here
