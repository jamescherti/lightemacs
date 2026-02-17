;;; le-prescient.el --- le-prescient -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; When prescient.el is used with Vertico, prescient.el enhances minibuffer
;; completion by dynamically reordering candidates based on frequency and
;; recency, making it faster to select commonly used options while preserving
;; consistent, predictable filtering.
;;
;; URL: https://github.com/radian-software/prescient.el

;;; Code:

(require 'lightemacs-package)

(lightemacs-package prescient
  :commands (prescient-completion-sort
             prescient-persist-mode)

  :init
  (add-hook 'lightemacs-after-init-hook #'prescient-persist-mode)

  ;; The .el extension is required; without it, Emacs may prompt for a file
  ;; encoding (e.g., UTF-8).
  (setq prescient-save-file (expand-file-name "prescient-save.el"
                                              user-emacs-directory))

  ;; Other parameters: TODO
  ;; (setq prescient-aggressive-file-save t)
  ;; (setq prescient-frequency-decay 0.997)
  ;; (setq prescient-frequency-threshold 0.05)
  ;; (setq prescient-history-length 200)
  ;; (setq prescient-sort-full-matches-first t)
  ;; (setq prescient-completion-highlight-matches nil)
  ;; (setq completion-preview-sort-function #'prescient-completion-sort)

  ;; Disabling length-based sorting. You might have noticed M-x is now sorting
  ;; all commands by shortest-first. If this is distracting to you, it can be
  ;; disabled!
  (setq prescient-sort-length-enable nil))

(provide 'le-prescient)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-prescient.el ends here
