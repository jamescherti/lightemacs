;;; le-core-cli-tools.el --- le-core-cli-tools -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lightemacs Modules Library

;;; Code:

(defvar lightemacs--ripgrep-executable (executable-find "rg" 'remote))
(defvar lightemacs--fdfind-executable
  (let ((fdfind-executable (executable-find "fdfind" 'remote)))
    (if fdfind-executable
        fdfind-executable
      (executable-find "fd" 'remote))))

(provide 'le-core-cli-tools)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-cli-tools.el ends here
