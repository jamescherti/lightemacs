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
             corfu-mode)

  :init
  (lightemacs-module-hooks corfu-global
    global-corfu-mode
    '(lightemacs-on-first-input-hook))

  (lightemacs-module-hooks corfu-local
    corfu-mode
    nil)

  (lightemacs-module-setq-maybe corfu
    tab-always-indent 'complete
    ;; Select first candidate, except for directories
    corfu-preselect 'directory
    global-corfu-modes '((not erc-mode
                              circe-mode
                              help-mode
                              gud-mode
                              eat-mode
                              vterm-mode)
                         t)
    corfu-auto nil
    corfu-auto-delay 0.24
    corfu-auto-prefix 2
    corfu-count 16
    corfu-max-width 120
    corfu-cycle lightemacs-cycle
    corfu-scroll-margin 1
    corfu-preview-current nil  ; Disable current candidate preview
    corfu-min-width 38

    ;; Keep the completion popup open at boundaries, such as when inserting a
    ;; space. Also, keep it open even if there are no matching candidates. This
    ;; is beneficial because you can continue editing without the popup closing,
    ;; allowing you to refine your input, apply filters, and more easily find
    ;; the candidate you are searching for.
    corfu-quit-at-boundary nil
    corfu-quit-no-match nil

    ;; Configure handling of exact matches
    corfu-on-exact-match nil)

  (when lightemacs-corfu-add-to-savehist
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history))))

(provide 'le-corfu)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-corfu.el ends here
