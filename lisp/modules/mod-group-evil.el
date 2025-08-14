;;; mod-group-evil.el --- mod-group-evil -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
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

;;; Evil

(lightemacs--load-modules '(evil
                            evil-commentary
                            evil-snipe
                            evil-surround))

(provide 'mod-group-evil)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-group-evil.el ends here
