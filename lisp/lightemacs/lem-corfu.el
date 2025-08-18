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
  (setq corfu-auto nil)
  (setq corfu-auto-delay 0.24)
  (setq corfu-auto-prefix 2)
  (setq corfu-count 15)
  (setq corfu-max-width 120)
  (setq corfu-cycle nil)  ; Enable cycling for `corfu-next/previous'
  (setq corfu-scroll-margin 1)
  (setq corfu-preselect 'directory)  ; Select the first candidate, except for directories
  (setq corfu-preview-current nil)  ; Disable current candidate preview
  (setq corfu-min-width 38)
  (setq corfu-separator ?\s)  ; Orderless field separator

  (setq corfu-quit-at-boundary 'separator)
  (setq corfu-quit-no-match 'separator)

  (setq tab-always-indent 'complete)

  ;; Configure handling of exact matches
  (setq corfu-on-exact-match nil))

(provide 'lem-corfu)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-corfu.el ends here
