;;; le-flavor-micro.el --- Group: Default modules -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The "micro" flavor.

;;; Code:

(eval-and-compile
  (require 'le-core-package-manager))

(lightemacs-load-modules
 '(le-theme
   le-keybindings
   le-default-settings
   le-dired
   le-dired-filter))

;;; Provide

(provide 'le-flavor-micro)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flavor-micro.el ends here
