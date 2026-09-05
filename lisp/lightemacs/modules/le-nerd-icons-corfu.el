;;; le-nerd-icons-corfu.el --- le-nerd-icons-corfu -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Requires: corfu
;;
;; `nerd-icons-corfu' adds a column of metadata icons to the left or right of
;; candidates in the popup menu.
;;
;; Instead of plain text items, candidates appear with visual markers indicating
;; their type:
;;
;;   Without margin formatter:
;;   +-----------------------------+
;;   | format-string               |
;;   | format-time-string          |
;;   | default-directory           |
;;   +-----------------------------+
;;
;;   With nerd-icons-corfu-formatter (margin added on the left):
;;   +-----------------------------+
;;   | [fn] format-string          |
;;   | [fn] format-time-string     |
;;   | [var] default-directory     |
;;   +-----------------------------+
;;
;; In graphical displays, `nerd-icons-corfu-formatter' replaces these text
;; markers with Nerd Font glyphs, allowing users to differentiate functions,
;; variables, keywords, and files at a glance.
;;
;; URL: https://github.com/LuigiPiucco/nerd-icons-corfu/

;;; Code:

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package nerd-icons-corfu
  :after corfu
  :commands (nerd-icons-corfu-formatter)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'le-nerd-icons-corfu)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-nerd-icons-corfu.el ends here
