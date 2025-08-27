;;; le-diminish.el --- le-diminish -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
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
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  diminish)

;; (with-eval-after-load 'eldoc
;;   (diminish 'eldoc-mode))
;;
;; (with-eval-after-load 'abbrev
;;   (diminish 'abbrev-mode))

(provide 'le-diminish)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-diminish.el ends here
