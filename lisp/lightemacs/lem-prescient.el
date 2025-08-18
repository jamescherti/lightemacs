;;; lem-prescient.el --- lem-prescient -*- lexical-binding: t -*-

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

;; prescient.el is a library for sorting and filtering lists of candidates, such
;; as those presented by packages like Vertico or Corfu.
;;
;; The main benefit of prescient.el is that it adaptively orders candidates
;; based on both frequency and recency of selection, making frequently used
;; options appear first without sacrificing predictable filtering results.

;; URL: https://github.com/radian-software/prescient.el

;;; Evil

(use-package prescient
  :commands (prescient-completion-sort
             prescient-persist-mode)
  :hook
  (after-init . prescient-persist-mode)

  :init
  (setq prescient-completion-highlight-matches nil)
  (setq prescient-save-file (expand-file-name "prescient-save.el"
                                              user-emacs-directory))
  (setq completion-preview-sort-function #'prescient-completion-sort)

  ;; Disabling length-based sorting. You might have noticed M-x is now sorting
  ;; all commands by shortest-first. If this is distracting to you, it can be
  ;; disabled!
  (setq prescient-sort-length-enable nil)

  ;; Whether certain literal filtering methods use character folding.
  ;; This affects the ‘literal’ and ‘literal-prefix’ filtering methods.
  (setq prescient-use-char-folding t)

  ;; Works well with `initialism'.
  (setq prescient-sort-full-matches-first t)

  ;; literal: the subquery must be a substring of the candidate.
  ;; Supports char folding.
  ;;
  ;; initialism: the subquery must match a substring of the initials
  ;; of the candidate.
  ;;
  ;; prefix: words (substrings of only word characters) match the
  ;; beginning of words found in the candidate, in order, separated by
  ;; the same non-word characters that separate words in the query.
  ;; This is similar to the completion style partial or what you get
  ;; from Ivy by default
  ;;
  ;; anchored: words are separated by capital letters or symbols, with
  ;; capital letters being the start of a new word. This is similar to
  ;; prefix, but allows for less typing.
  ;;
  ;; fuzzy: the characters of the subquery must match some subset of
  ;; those of the candidate, in the correct order but not necessarily
  ;; contiguous.
  ;;
  ;; regexp: the subquery is interpreted directly as a regular
  ;; expression. (Try ^p\w+d)
  ;; (prescient-filter-method '(literal initialism prefix regexp fuzzy))
  (setq prescient-filter-method '(literal regexp initialism)))

(provide 'lem-prescient)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-prescient.el ends here
