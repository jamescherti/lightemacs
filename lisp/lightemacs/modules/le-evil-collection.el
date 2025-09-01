;;; le-evil-collection.el --- le-evil-collection -*- lexical-binding: t -*-

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
;; This configures the evil-collection package.
;;
;; URL: https://github.com/emacs-evil/evil-collection

;;; Code:

(eval-and-compile
  (require 'lightemacs)
  (require 'le-evil)
  (require 'le-diminish))

(eval-and-compile
  ;; This has to be defined before evil
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-collection-setup-minibuffer t))

(lightemacs-use-package
  evil-collection
  :if (not noninteractive)
  :diminish (evil-collection-unimpaired-mode)
  :functions evil-collection-init
  :after evil
  :config
  (evil-collection-init))

(provide 'le-evil-collection)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-evil-collection.el ends here
