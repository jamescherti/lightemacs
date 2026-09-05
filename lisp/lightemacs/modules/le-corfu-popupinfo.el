;;; le-corfu-popupinfo.el --- le-corfu-popupinfo -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Requirement: corfu
;;
;; The `corfu-popupinfo' package displays a side window with extra details when
;; you select a completion candidate. For example, when completing a function
;; name like `find-file', the popup automatically shows its docstring or the
;; file path where the function is defined.
;;
;; Key bindings:
;; - `corfu-popupinfo-toggle'
;; - `corfu-popupinfo-location'
;; - `corfu-popupinfo-documentation'
;;
;; URL: https://github.com/minad/corfu

;;; Code:

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package corfu-popupinfo
  :ensure nil ; This is part of corfu
  :commands corfu-popupinfo-mode
  :init
  (lightemacs-module-setq-maybe corfu-popupinfo
    corfu-popupinfo-delay '(1.25 . 0.5)
    corfu-popupinfo-max-width 70
    corfu-popupinfo-max-height 15)

  (lightemacs-module-hooks corfu-popupinfo
    corfu-popupinfo-mode
    '(corfu-mode-hook)))

(provide 'le-corfu-popupinfo)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-corfu-popupinfo.el ends here
