;;; le-dtrt-indent.el --- le-dtrt-indent -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This dtrt-indent package provides functions to detects the indentation offset
;; used in existing source code files and automatically adjusts Emacs settings
;; accordingly, thereby simplifying the editing of files created in external
;; environments.
;;
;; URL: https://github.com/jscheid/dtrt-indent

;;; Code:

(use-package dtrt-indent
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight))

(provide 'le-dtrt-indent)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-dtrt-indent.el ends here
