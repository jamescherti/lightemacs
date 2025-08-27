;;; le-vertico.el --- le-vertico -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
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

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  vertico
  :commands (vertico-mode
             vertico-next
             vertico-previous
             vertico-exit-input)
  :functions vertico--index

  :init
  (setq vertico-scroll-margin 0)
  (setq vertico-count 16)
  (setq vertico-resize 'grow-only)
  (setq vertico-cycle lightemacs-cycle)
  (setq vertico-count-format nil) ; No prefix with number of entries

  (lightemacs-define-mode-hook-list vertico-mode
                                    '(lightemacs-on-first-input-hook))

  (lightemacs-define-keybindings vertico
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-d") #'vertico-exit-input)
      (define-key vertico-map (kbd "C-j") #'vertico-next)
      (define-key vertico-map (kbd "C-k") #'vertico-previous)))

  :config
  ;; Prefix current candidate with arrow
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (defvar +vertico-current-arrow t)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context
          ((and +vertico-current-arrow
                (not (bound-and-true-p vertico-flat-mode)))
           (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (= vertico--index index)
        (concat #("► " 0 2 (face vertico-current)) cand)
      (concat #("_ " 0 1 (display " ")) cand))))

;;; Provide
(provide 'le-vertico)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-vertico.el ends here
