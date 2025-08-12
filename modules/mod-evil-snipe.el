;;; mod-evil-snipe.el --- mod-evil-snipe -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; evil-snipe provides two-character motions for rapid navigation within text,
;; similar to Evil’s built-in f/F/t/T commands, but with incremental
;; highlighting of potential targets as you type. By default, s (forward) and
;; S (backward) are bound to evil-snipe-s and evil-snipe-S, respectively.
;;
;; Pressing s in normal mode prompts you to type two characters, then jumps the
;; cursor to the nearest matching occurrence while highlighting all matches
;; incrementally.
;;
;; URL: https://github.com/hlissner/evil-snipe

;;; Code:

(use-package evil-snipe
  :after evil
  :hook (evil-mode . evil-snipe-mode))

(provide 'mod-evil-snipe)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-evil-snipe.el ends here
