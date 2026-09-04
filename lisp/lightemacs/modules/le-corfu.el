;;; le-corfu.el --- le-corfu -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.
;;
;; URL: https://github.com/minad/corfu

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'lightemacs-module)

(defvar lightemacs-corfu-add-to-savehist t)

(lightemacs-use-package corfu
  :commands (global-corfu-mode
             corfu-mode
             corfu-history-mode)

  :init
  (lightemacs-module-hooks corfu-global
    global-corfu-mode
    '(lightemacs-on-first-input-hook))

  (lightemacs-module-hooks corfu-local
    corfu-mode
    nil)

  (lightemacs-module-setq-maybe corfu
    corfu-preselect 'directory
    global-corfu-modes '((not erc-mode
                              circe-mode
                              help-mode
                              gud-mode
                              eat-mode
                              term-mode
                              vterm-mode)
                         t)
    corfu-auto nil ; Security and performance default
    corfu-auto-delay 0.24
    corfu-auto-prefix 2
    corfu-count 15
    corfu-max-width 100
    corfu-cycle lightemacs-cycle
    corfu-scroll-margin 2
    corfu-preview-current nil  ; Disable current candidate preview
    corfu-min-width 20

    ;; Setting `corfu-quit-at-boundary' to nil prevents Corfu from closing the
    ;; completion popup when the cursor reaches or crosses the completion
    ;; boundary defined by the active completion-at-point-function (Capf).
    ;; You can freely type spaces or delimiter characters to supply multiple
    ;; filtering terms to Orderless without the session terminating.
    corfu-quit-at-boundary (if corfu-auto 'separator nil)

    ;; Prevent the popup from closing when a typo results in zero matches.
    ;; This allows using Backspace to correct the input and restore candidates.
    corfu-quit-no-match (if corfu-auto 'separator nil))

  (when lightemacs-corfu-add-to-savehist
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history)))

  ;; Corfu: Corfu candidate lists are typically shorter, so its default sorting
  ;; is simpler. To get it to sort by recency, you must explicitly enable
  ;; corfu-history-mode.
  ;;
  ;; Comparison with Corfu Prescient: While corfu-history sorts purely by
  ;; recency (your most recent selection goes to the top), Prescient tracks both
  ;; recency and frequency. With corfu-history, selecting a rare candidate once
  ;; will immediately place it above your most frequently used candidates.
  ;; Prescient prevents this by keeping high-frequency candidates near the top
  ;; even if they were not the absolute last item selected.
  (lightemacs-module-hooks corfu-history corfu-history-mode '()))

(provide 'le-corfu)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-corfu.el ends here
