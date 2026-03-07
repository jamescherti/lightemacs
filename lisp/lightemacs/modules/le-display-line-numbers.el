;;; le-display-line-numbers.el --- le-display-line-numbers -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Show line numbers on the left side of the buffer. The numbers update
;; automatically as you add, remove, or scroll lines, but they don’t change the
;; actual text.

;;; Code:

(require 'lightemacs-use-package)

(lightemacs-use-package display-line-numbers
  :ensure nil
  :commands (global-display-line-numbers-mode
             display-line-numbers-mode)
  :hook ((prog-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode)))

(provide 'le-display-line-numbers)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-display-line-numbers.el ends here
