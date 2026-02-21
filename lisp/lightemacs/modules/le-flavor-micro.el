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

(require 'lightemacs-module)

(lightemacs-module-load
 '(le-theme

   ;; Lightemacs keybindings
   le-default-keybindings

   ;; Default settings (minibuffer, Emacs...) Configure enhanced default
   ;; settings, including improved defaults, backup files, warnings to ignore, a
   ;; minibuffer depth indicator, window behavior...
   le-default-settings

   ;; Configure `dired' to hide details such as file ownership and permissions,
   ;; and to group directories first.
   le-dired))

;;; Provide

(provide 'le-flavor-micro)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flavor-micro.el ends here
