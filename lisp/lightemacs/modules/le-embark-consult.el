;;; le-embark-consult.el --- le-embark-consult -*- lexical-binding: t -*-

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

(require 'use-package)
(require 'le-embark)
(require 'le-consult)

(use-package embark-consult
  :demand t
  :after (embark consult))

;;; Provide
(provide 'le-embark-consult)

;;; le-embark-consult.el ends here
