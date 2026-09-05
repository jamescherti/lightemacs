;;; le-evil-matchit.el --- le-evil-matchit -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; NOTE: Despite its name, the `evil-matchit' package is fully compatible with
;; vanilla Emacs and does NOT require `evil-mode'.
;;
;; `evil-matchit' enables jumping between language-specific matching tags and
;; control structures (such as HTML/XML tags, if/else/fi, def/end, and loop
;; blocks).
;;
;; It provides out-of-the-box support for a wide array of syntaxes, ranging from
;; Python indentation blocks and Bash shell scripting constructs to Git merge
;; conflicts.
;;
;; Although designed to integrate cleanly with `evil-mode', the package operates
;; entirely independently and offers native commands for standard usage.
;; Additionally, it features an accessible Elisp API, enabling developers to
;; quickly write custom matching rules and integrate third-party jump functions
;; for any unsupported languages.
;;
;; URL: https://github.com/redguardtoo/evil-matchit

;;; Code:

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package evil-matchit
  :commands (evil-matchit-mode
             global-evil-matchit-mode)
  :init
  (lightemacs-module-hooks evil-matchit-global
    global-evil-matchit-mode
    '(lightemacs-after-init-hook))

  (lightemacs-module-hooks evil-matchit-local
    evil-matchit-mode
    '())

  (with-eval-after-load 'evil
    (require 'evil-matchit-evil-setup)))

(provide 'le-evil-matchit)

;;; le-evil-matchit.el ends here
