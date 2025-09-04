;;; le-cape.el --- le-cape -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
;;
;; URL: https://github.com/minad/cape

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package cape
  :commands (cape-abbrev
             cape-dabbrev
             cape-dict
             cape-elisp-block
             cape-elisp-symbol
             cape-file
             cape-history
             cape-line)

  :bind
  ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+

  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'le-cape)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-cape.el ends here
