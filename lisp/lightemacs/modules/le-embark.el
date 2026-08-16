;;; le-embark.el --- le-embark -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package embark
  :commands (embark-act
             embark-export
             embark-collect
             embark-dwim
             embark-bindings
             embark-prefix-help-command
             embark-eldoc-first-target)

  :init
  (lightemacs-module-bind embark
    (keymap-global-set "C-." #'embark-act)
    (keymap-global-set "C-;" #'embark-dwim)
    (keymap-global-set "C-h B" #'embark-bindings)
    (keymap-global-set "C-c C-;" #'embark-export)
    (keymap-global-set "C-c C-l" #'embark-collect))

  (lightemacs-module-setq-maybe embark
    ;; Replace the key help with a completing-read interface
    prefix-help-command #'embark-prefix-help-command
    embark-quit-after-action nil
    embark-mixed-indicator-delay 1
    embark-verbose-indicator-display-action
    '(display-buffer-at-bottom (window-height . fit-window-to-buffer)))

  ;; Hide the mode line of the Embark live/completions buffers
  (push '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
          nil
          (window-parameters (mode-line-format . none)))
        display-buffer-alist))

(provide 'le-embark)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-embark.el ends here
