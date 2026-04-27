;;; le-group-code-folding.el --- group-code-folding -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure Emacs code folding.
;;
;; This configuration is similar to:
;; https://www.jamescherti.com/emacs-the-definitive-guide-to-code-folding/

;;; Code:

(require 'lightemacs-module)

(lightemacs-module-load
 '(le-outline
   le-outline-indent
   le-hideshow
   le-treesit-fold
   le-kirigami))

(provide 'le-group-code-folding)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-group-code-folding.el ends here
