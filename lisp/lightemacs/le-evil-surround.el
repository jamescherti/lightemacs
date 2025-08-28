;;; le-evil-surround.el --- le-evil-surround -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Evil-surround enables text surrounding in visual state using S<textobject> or
;; gS<textobject>.
;;
;; For example, selecting text and pressing S" will wrap it in double quotes.
;;
;; URL: https://github.com/emacs-evil/evil-surround

;;; Code:

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package)
  (require 'le-evil))

(lightemacs-use-package
  evil-surround
  :after evil
  :commands (evil-Surround-edit
             evil-surround-edit
             evil-surround-region
             evil-surround-mode
             global-evil-surround-mode)
  :init
  (lightemacs-define-mode-add-hook-to global-evil-surround-mode
                                      '(evil-mode-hook))
  :config
  (global-evil-surround-mode))

(provide 'le-evil-surround)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-evil-surround.el ends here
