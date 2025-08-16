;;; mod-goto-chg.el --- mod-goto-chg -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Navigate to the most recent edit in the buffer using `goto-last-change' or
;; `goto-last-change-reverse'. Commonly used in `evil-mode' for the motions 'g;'
;; and 'g,', as well as for the last-change register '.'.
;;
;; URL: https://github.com/emacs-evil/goto-chg

;;; Code:

(use-package goto-chg
  :commands (goto-last-change
             goto-last-change-reverse))

(provide 'mod-goto-chg)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-goto-chg.el ends here
