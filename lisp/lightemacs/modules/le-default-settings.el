;;; le-default-settings.el --- le-default-settings -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Miscellaneous Lightemacs default settings.

;;; Code:

;;; Require

;; Load `lightemacs--ripgrep-executable' and `lightemacs--fdfind-executable'
(require 'le-core-cli-tools)

;;; Misc

;; TODO: move to m.e.?
(setq eldoc-documentation-strategy #'eldoc-documentation-compose)

(setq byte-compile-warnings
      (if init-file-debug
          t
        ;; free-vars: warns about the use of free (unbound) variables. This is
        ;; almost always a sign of a typo or a bug.
        ;;
        ;; unresolved: warns when a function or variable cannot be found at
        ;; compile time. This can indicate a missing require or an undefined
        ;; symbol.
        ;;
        ;; noruntime – warns when runtime evaluation of code is disabled, e.g.,
        ;; using defvar or defun incorrectly in a macro context.
        ;;
        ;; obsolete – optional, but useful if you want to catch usage of
        ;; deprecated functions or variables.
        ;; '(not free-vars unresolved noruntime)
        t

        ;; (byte-compile-warnings . (not free-vars unresolved mapcar constants))
        ;; (setq byte-compile-warnings '(not nresolved free-vars callargs redefine obsolete noruntime cl-functions interactive-only))
        ;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))
        ;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
        ;; (setq byte-compile-warnings '(not lexical))
        ;; (setq warning-suppress-types '((lexical-binding)))
        ;; (setq warning-minimum-level :error)

        ))
(setq warning-minimum-level (if init-file-debug :warning :error))

;; Alternative: (setq-default display-fill-column-indicator-character ?┊)
(setq-default display-fill-column-indicator-character ?\N{U+2502})

(setq treesit-font-lock-level 4) ; Max: 4

;;; Autosave

;;; Tools (ripgrep and fd)

(when lightemacs--ripgrep-executable
  (setq xref-search-program 'ripgrep))

;;; VC

(setq vc-git-print-log-follow t)

;;; Minibuffer

(setq minibuffer-default-prompt-format " [default %s]")
;; TODO use macro?
(add-hook 'lightemacs-on-first-input-hook #'minibuffer-depth-indicate-mode)

;;; Mode line

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;;; Frame

;; TODO use macro?
(add-hook 'after-init-hook #'window-divider-mode)

;;; Diminish Eldoc, Abbref...

(with-eval-after-load 'diminish
  (with-eval-after-load 'eldoc
    (when (fboundp 'diminish)
      (diminish 'eldoc-mode)))

  (with-eval-after-load 'abbrev
    (when (fboundp 'diminish)
      (diminish 'abbrev-mode))))

(provide 'le-default-settings)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-default-settings.el ends here
