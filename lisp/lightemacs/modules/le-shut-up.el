;;; le-shut-up.el --- le-shut-up -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The shut-up package suppresses output from functions that normally print to
;; the *Messages* buffer or to the echo area.
;;
;; It provides a macro called `shut-up' that temporarily silences messages while
;; evaluating its body. This is useful when running code that would otherwise
;; clutter the user's *Messages* buffer with unnecessary output.
;;
;; URL: https://github.com/cask/shut-up

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package shut-up)

(provide 'le-shut-up)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-shut-up.el ends here
