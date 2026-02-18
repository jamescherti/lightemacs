;;; le-stripspace.el --- le-stripspace -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The stripspace Emacs package provides stripspace-local-mode, a minor mode
;; that automatically removes trailing whitespace and blank lines at the end of
;; the buffer when saving.

;;; Code:

(require 'lightemacs-module)

(lightemacs-module-package stripspace
  :commands stripspace-local-mode)

(lightemacs-module-hooks stripspace
  stripspace-local-mode
  '(prog-mode-hook
    text-mode-hook
    conf-mode-hook))

(provide 'le-stripspace)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-stripspace.el ends here
