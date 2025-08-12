;;; mod-corfu.el --- mod-corfu -*- lexical-binding: t -*-

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

  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2)
  (corfu-count 14)
  (corfu-cycle nil)  ; Enable cycling for `corfu-next/previous'
  (corfu-scroll-margin 1)
  (corfu-quit-no-match nil)  ; Never quit, even if there is no match.
  (corfu-preselect 'directory)  ; Select the first candidate, except for directories
  (corfu-preview-current nil)  ; Disable current candidate preview
  (corfu-min-width 38)
  (corfu-separator ?\s)  ; Orderless field separator

  ;; Prevent quitting at completion boundaries
  ;; (such as when pressing "-" in sh-mode)
  (corfu-quit-at-boundary nil)

  ;; Configure handling of exact matches
  (corfu-on-exact-match 'insert))

(provide 'mod-corfu)

;;; mod-corfu.el ends here
