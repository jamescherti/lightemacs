;;; le-dtrt-indent.el --- le-dtrt-indent -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This dtrt-indent package provides functions to detects the indentation offset
;; used in existing source code files and automatically adjusts Emacs settings
;; accordingly, thereby simplifying the editing of files created in external
;; environments.
;;
;; URL: https://github.com/jscheid/dtrt-indent

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'lightemacs-module)

(lightemacs-use-package dtrt-indent
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight)

  :init
  ;; TODO write a better major-mode-after-body hook
  (lightemacs-module-hooks dtrt-indent
    dtrt-indent-mode
    '(prog-mode-hook
      text-mode-hook))

  (setq dtrt-indent-verbosity (if init-file-debug 1 0))
  (setq dtrt-indent-max-lines 1900)  ; Faster

  (add-hook 'after-init-hook #'dtrt-indent-global-mode)

  ;; We're using `dtrt-indent'
  (setq python-indent-guess-indent-offset nil)

  ;; By default, `dtrt-indent' detects SMIE-based modes and lets
  ;; `smie-config-guess' handle indentation. However, `dtrt-indent' also sets
  ;; additional variables that SMIE does not (for example,
  ;; `dtrt-indent-hook-generic-mapping-list'). Setting this option to non-nil
  ;; forces `dtrt-indent' to run in SMIE-based modes as well, ensuring these
  ;; extra settings are applied.
  (setq dtrt-indent-run-after-smie t))

(provide 'le-dtrt-indent)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-dtrt-indent.el ends here
