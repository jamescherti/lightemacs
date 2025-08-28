;;; le-highlight-defined.el --- le-highlight-defined -*- lexical-binding: t -*-

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
;; URL: https://github.com/Fanael/highlight-defined

;;; Code:

(eval-and-compile
  (require 'lightemacs)
  (require 'use-package))

(lightemacs-use-package
  highlight-defined
  :commands highlight-defined-mode
  :init
  (lightemacs-define-mode-add-hook-to highlight-defined-mode
                                      '(emacs-lisp-mode-hook)))

(provide 'le-highlight-defined)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-highlight-defined.el ends here
