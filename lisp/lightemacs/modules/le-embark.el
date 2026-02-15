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

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package embark
  :commands (embark-act
             embark-export
             embark-collect
             embark-dwim
             embark-bindings
             embark-prefix-help-command
             embark-eldoc-first-target)

  :bind
  (("C-."     . embark-act)
   ("C-;"     . embark-dwim)
   ("C-h B"   . embark-bindings)
   ("C-c C-;" . embark-export)
   ("C-c C-l" . embark-collect))

  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)


  (setq embark-quit-after-action nil)
  (setq embark-mixed-indicator-delay 1)
  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom (window-height . fit-window-to-buffer)))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; Provide
(provide 'le-embark)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-embark.el ends here
