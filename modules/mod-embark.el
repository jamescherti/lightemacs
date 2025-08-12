;;; mod-embark.el --- mod-embark -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Vertico, Consult, and Embark collectively enhance Emacs' completion and
;; navigation capabilities.
;;
;; Embark integrates with these tools to provide context-sensitive actions and
;; quick access to commands based on the current selection, further improving
;; user efficiency and workflow within Emacs. Together, they create a cohesive
;; and powerful environment for managing completions and interactions.
;;
;; URL: https://github.com/oantolin/embark

;;; Code:

(use-package embark
  :commands (embark-act
             embark-export
             embark-collect
             embark-dwim
             embark-bindings
             embark-prefix-help-command
             embark-eldoc-first-target)
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (embark-quit-after-action nil)
  (embark-mixed-indicator-delay 1)
  (embark-verbose-indicator-display-action
   '(display-buffer-at-bottom (window-height . fit-window-to-buffer))))

;;; Provide
(provide 'mod-embark)

;;; mod-embark.el ends here
