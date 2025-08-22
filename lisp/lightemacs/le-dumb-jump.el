;;; le-dumb-jump.el --- le-dumb-jump -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Dumb-jump provides context-aware 'go to definition' functionality for 50+
;; programming languages without requiring a language server. It works by using
;; simple heuristics and regular expression searches to locate the definitions
;; of functions, variables, and symbols across project files. Unlike more
;; sophisticated language-aware tools, `dumb-jump' does not parse code
;; semantically, which makes it lightweight and fast, but sometimes less
;; precise. It integrates with popular navigation packages like `xref', allowing
;; users to jump to definitions or references.
;;
;; URL: https://github.com/jacktasia/dumb-jump

;;; Code:

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-lib)

(use-package dumb-jump
  :commands dumb-jump-xref-activate

  :init
  ;; Register `dumb-jump' as an xref backend so it integrates with
  ;; `xref-find-definitions'
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  ;; (setq dumb-jump-quiet t)
  (setq dumb-jump-aggressive nil)

  ;; Number of seconds a rg/grep/find command can take before being warned to
  ;; use ag and config.
  (setq dumb-jump-max-find-time 3)

  ;; Use `completing-read' so that selection of jump targets integrates with the
  ;; active completion framework (e.g., Vertico, Ivy, Helm, Icomplete),
  ;; providing a consistent minibuffer-based interface whenever multiple
  ;; definitions are found.
  (setq dumb-jump-selector 'completing-read)

  ;; If ripgrep is available, force `dumb-jump' to use it because it is
  ;; significantly faster and more accurate than the default searchers (grep,
  ;; ag, etc.).
  (when lightemacs--ripgrep-executable
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump-prefer-searcher 'rg))

  :config
  (with-eval-after-load 'project
    (cl-callf cl-union dumb-jump-project-denoters project-vc-extra-root-markers)))

(provide 'le-dumb-jump)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-dumb-jump.el ends here
