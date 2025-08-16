;;; mod-highlight-defined.el --- mod-highlight-defined -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Enable `highlight-defined', a minor mode that highlights defined Emacs Lisp
;; symbols in `emacs-lisp-mode' buffers.
;;
;; URL: https://github.com/jamescherti/highlight-defined

;;; Code:

(use-package highlight-defined
  :commands highlight-defined-mode
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(provide 'mod-highlight-defined)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-highlight-defined.el ends here
