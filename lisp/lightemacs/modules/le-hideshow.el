;;; le-hideshow.el --- le-hideshow -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `hs-minor-mode' parses buffer syntax to accurately detect the start and end
;; of blocks. It is the best tool for C-style languages, or anything using
;; braces {} and explicit block structures like sh/Bash shell scripts.

;;; Code:

;;; Hooks

(require 'lightemacs-module)

;;; lightemacs-module-hooks

(lightemacs-module-hooks hs-minor
  hs-minor-mode
  '(;; Systems and General Purpose
    c-mode-hook
    c++-mode-hook
    java-mode-hook
    rust-mode-hook
    go-mode-hook
    ruby-mode-hook

    ;; Web and Frontend
    js-mode-hook
    typescript-mode-hook
    css-mode-hook

    ;; Scripting, Data, and Infrastructure
    sh-mode-hook
    json-mode-hook
    lua-mode-hook
    nxml-mode-hook
    html-mode-hook))

(provide 'le-hideshow)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-hideshow.el ends here
