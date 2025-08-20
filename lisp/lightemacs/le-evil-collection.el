;;; le-evil-collection.el --- le-evil-collection -*- no-byte-compile: t; lexical-binding: t -*-

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

;;; evil-collection

(require 'le-evil)
(require 'le-diminish)


(use-package evil-collection
  :if (not noninteractive)
  :diminish (evil-collection-unimpaired-mode)
  :after evil
  :functions evil-collection-init
  :config
  (evil-collection-init))

(provide 'le-evil-collection)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-evil-collection.el ends here
