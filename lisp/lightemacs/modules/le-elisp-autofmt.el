;;; le-elisp-autofmt.el --- le-elisp-autofmt -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `elisp-autofmt' provides an automatic code formatter for Emacs Lisp. It
;; includes commands to format the entire buffer or a selected region, as well
;; as a minor mode to apply formatting automatically.
;;
;;  It includes the `elisp-autofmt-mode' minor mode, along with
;; `elisp-autofmt-buffer' and `elisp-autofmt-region' commands to format the
;; entire buffer or selection.
;;
;; URL: https://codeberg.org/ideasman42/emacs-elisp-autofmt

;;; Code:

(require 'lightemacs-module)

(lightemacs-use-package elisp-autofmt
  :commands (elisp-autofmt-mode
             elisp-autofmt-buffer
             elisp-autofmt-region))

(provide 'le-elisp-autofmt)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-elisp-autofmt.el ends here
