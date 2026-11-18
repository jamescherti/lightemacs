;;; le-vertico.el --- le-vertico -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Vertico provides a vertical completion interface, making it easier to
;; navigate and select from completion candidates (e.g., when M-x is pressed).
;;
;; Vertico, Consult, and Embark collectively enhance Emacs' completion and
;; navigation capabilities.
;;
;; URL: https://github.com/minad/vertico

;;; Code:

(defvar lightemacs-vertico-current-arrow t)

(require 'lightemacs)

(use-package vertico
  :commands (vertico-mode
             vertico-next
             vertico-previous
             vertico-exit-input)
  :functions (vertico--index
              vertico-sort-history-length-alpha)

  :init
  (lightemacs-module-hooks vertico
    vertico-mode
    '(lightemacs-on-first-input-hook))

  (lightemacs-module-setq-maybe vertico
    vertico-scroll-margin 0
    vertico-count 16
    vertico-resize 'grow-only
    vertico-cycle lightemacs-cycle
    vertico-count-format nil) ; No prefix with number of entries

  :preface
  (defun lightemacs-vertico-format-candidate-advice (orig-fun
                                                     cand prefix
                                                     suffix index start)
    (let ((formatted-cand (funcall orig-fun cand prefix suffix index start)))
      (if (and lightemacs-vertico-current-arrow
               (not (bound-and-true-p vertico-flat-mode)))
          (if (= vertico--index index)
              (concat #("► " 0 2 (face vertico-current)) formatted-cand)
            (concat #("_ " 0 1 (display " ")) formatted-cand))
        formatted-cand)))

  :config
  ;; Vertico's default `vertico-sort-function' checks
  ;; (fboundp 'vertico-sort-history-length-alpha) at load time and falls back
  ;; to nil if the function is not yet defined. When loading Vertico directly
  ;; from `load-path' without a package manager to generate autoloads,
  ;; `vertico-sort-history-length-alpha' is not loaded yet, leaving
  ;; `vertico-sort-function' set to nil. Load `vertico-sort' explicitly and
  ;; restore the default history-based sorting function.
  (require 'vertico-sort)
  (lightemacs-module-setq-maybe vertico
    vertico-sort-function #'vertico-sort-history-length-alpha)

  ;; Prefix current candidate with arrow
  (advice-add 'vertico--format-candidate :around
              #'lightemacs-vertico-format-candidate-advice))

(provide 'le-vertico)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-vertico.el ends here
