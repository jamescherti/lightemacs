;;; le-evil-visualstar.el --- le-evil-visualstar -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Enables evil-visualstar which allows searching for the current visual
;; selection using * or #.
;;
;; Usage: Create a visual selection with v or V, then press * to search forward
;; or # to search backward. When evil-visualstar/persistent is non-nil, visual
;; state remains active, enabling repeated searches without reselecting the
;; text.
;;
;; URL: https://github.com/bling/evil-visualstar

;;; Code:

(require 'lightemacs-use-package)

(lightemacs-use-package evil-visualstar
  :commands global-evil-visualstar-mode
  :after evil
  :config
  (global-evil-visualstar-mode))

(provide 'le-evil-visualstar)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-evil-visualstar.el ends here
