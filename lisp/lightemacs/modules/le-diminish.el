;;; le-diminish.el --- le-diminish -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Hides or abbreviates mode indicators in the Emacs
;; mode line for a cleaner display
;;
;; URL: https://github.com/myrjola/diminish.el/

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package diminish)

(provide 'le-diminish)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-diminish.el ends here
