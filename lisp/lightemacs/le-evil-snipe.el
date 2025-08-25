;;; le-evil-snipe.el --- le-evil-snipe -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Evil-snipe provides two-character motions for rapid navigation within text,
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

(require 'lightemacs)
(require 'le-diminish)

(lightemacs-use-package
  evil-snipe
  :diminish evil-snipe-local-mode
  :commands evil-snipe-mode
  :init
  ;; Enable smart case sensitivity: uppercase searches are case-sensitive,
  ;; lowercase are case-insensitive
  (setq evil-snipe-smart-case t)

  ;; Restrict repeat operations to the visible region only
  (setq evil-snipe-repeat-scope 'visible)

  ;; Allow character folding so equivalent characters match (e.g.,
  ;; accents/diacritics)
  (setq evil-snipe-char-fold t))

;; TODO: on first input
(lightemacs-define-mode-hook-list evil-snipe-mode
                                  '(evil-mode-hook))

(provide 'le-evil-snipe)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-evil-snipe.el ends here
