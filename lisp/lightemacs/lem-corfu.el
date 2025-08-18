;;; lem-corfu.el --- lem-corfu -*- lexical-binding: t -*-

;; Author: James Cherti
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

(use-package corfu
  :commands (global-corfu-mode
             corfu-mode)

  :hook
  (after-init . global-corfu-mode)
  ;; TODO: use on-first-input
  ;; (on-first-input . global-corfu-mode)

  :init
  (setq tab-always-indent 'complete)

  (setq corfu-auto nil)
  (setq corfu-preselect 'prompt)  ; Select first candidate, except for directories

  (setq global-corfu-modes '((not erc-mode
                                  circe-mode
                                  help-mode
                                  gud-mode
                                  eat-mode
                                  vterm-mode)
                             t))
  (setq corfu-auto-delay 0.24)
  (setq corfu-auto-prefix 2)
  (setq corfu-count 16)
  (setq corfu-max-width 120)
  (setq corfu-cycle lightemacs-cycle)
  (setq corfu-scroll-margin 1)
  (setq corfu-preview-current nil)  ; Disable current candidate preview
  (setq corfu-min-width 38)

  ;; Keep the completion popup open at boundaries, such as when inserting a space.
  ;; Also, keep it open even if there are no matching candidates. This is beneficial because
  ;; you can continue editing without the popup closing, allowing you to refine your input,
  ;; apply filters, and more easily find the candidate you are searching for.
  (setq corfu-quit-at-boundary nil)
  (setq corfu-quit-no-match nil)

  ;; Configure handling of exact matches
  (setq corfu-on-exact-match nil)

  :config
  (require 'corfu-history))

(provide 'lem-corfu)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-corfu.el ends here
