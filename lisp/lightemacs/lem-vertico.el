;;; lem-vertico.el --- lem-vertico -*- lexical-binding: t -*-

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

(use-package vertico
  :commands vertico-mode
  :bind (:map vertico-map
              ("C-d" . vertico-exit-input)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))

  :hook
  (after-init . vertico-mode)
  ;; TODO: use on-first-input
  ;; (add-hook 'lightemacs-on-first-input-hook #'vertico-mode)

  :init
  (setq vertico-scroll-margin 0)
  (setq vertico-count 16)
  (setq vertico-resize 'grow-only)
  (setq vertico-cycle t)
  (setq vertico-count-format nil) ; No prefix with number of entries

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
(provide 'lem-vertico)

;;; lem-vertico.el ends here
