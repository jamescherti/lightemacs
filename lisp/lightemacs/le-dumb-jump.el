;;; le-dumb-jump.el --- le-dumb-jump -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Dumb-jump provides context-aware 'go to definition' functionality for
;; multiple programming languages without requiring a language server. It works
;; by using simple heuristics and regular expression searches to locate the
;; definitions of functions, variables, and symbols across project files. Unlike
;; more sophisticated language-aware tools, `dumb-jump' does not parse code
;; semantically, which makes it lightweight and fast, but sometimes less
;; precise. It integrates with popular navigation packages like `xref', allowing
;; users to jump to definitions, references, or implementations with minimal
;; configuration.
;;
;; URL: https://github.com/jacktasia/dumb-jump

;;; Code:

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-lib)

(use-package dumb-jump
  :commands dumb-jump-xref-activate

  :init
  ;; Enable the xref backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  ;; (add-hook 'after-init-hook #'dumb-jump-mode)

  ;; Xref can be customized to use completing-read to select a target. That way
  ;; a completion framework of your choice (Icomplete, Helm, Ivy, ...) will be
  ;; used instead of the default pop-up buffer. To do this, evaluate
  ;;
  ;; Note that the function xref-show-definitions-completing-read requires at
  ;; least Xref 1.1.0. This can either be downloaded from ELPA or is bundled
  ;; with Emacs 28.1 or newer.
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)

  ;; (setq dumb-jump-quiet t)
  ;; (setq dumb-jump-aggressive t)
  ;; (setq dumb-jump-selector 'completing-read)
  ;; (setq dumb-jump-max-find-time 5)
  ;; (add-to-list 'dumb-jump-project-denoters ".project")

  (when lightemacs--ripgrep-executable
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump-prefer-searcher 'rg)))

(provide 'le-dumb-jump)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-dumb-jump.el ends here
