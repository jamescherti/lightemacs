;;; le-group-evil.el --- group-evil -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Evil is an extensible vi layer for Emacs. It emulates the main features of
;; Vim, and provides facilities for writing custom extensions.
;;
;; This configures the packages evil and evil-collection.
;;
;; URL: https://github.com/emacs-evil/evil

;;; Code:

(require 'lightemacs-module)

(lightemacs-module-load
 '(;; evil and evil-collection
   le-evil
   le-evil-collection

   ;; Comment or uncomment text in Normal or Visual mode
   ;; by pressing 'gc'.
   le-evil-commentary

   ;; Evil-surround enables text surrounding in visual state using S<textobject>
   ;; or gS<textobject>. For example, selecting text and pressing S" will wrap
   ;; it in double quotes.
   le-evil-surround

   ;; Goto-chg is Used by `evil-mode' for the motions 'g;' and 'g,' as well as
   ;; for the last-change register '.'.
   le-goto-chg))

(provide 'le-group-evil)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-group-evil.el ends here
