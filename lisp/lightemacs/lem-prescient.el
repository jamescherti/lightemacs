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
  (setq prescient-save-file (expand-file-name "prescient-save.el"
                                              user-emacs-directory))

  ;; TODO
  ;; (setq prescient-completion-highlight-matches nil)
  ;; (setq completion-preview-sort-function #'prescient-completion-sort)
  ;; (setq prescient-sort-full-matches-first t)
  ;; (setq prescient-use-char-folding t)

  ;; Disabling length-based sorting. You might have noticed M-x is now sorting
  ;; all commands by shortest-first. If this is distracting to you, it can be
  ;; disabled!
  (setq prescient-sort-length-enable nil))

(provide 'lem-prescient)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-prescient.el ends here
