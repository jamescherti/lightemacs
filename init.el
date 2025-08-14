;;; init.el --- Init -*- lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The Lightemacs project is an Emacs framework.

;;; Code:

;; Load minimal-emacs.d init.el
(if (fboundp 'lightemacs--load-init-file)
    (lightemacs--load-init-file "init.el")
  (error "The early-init.el file was not loaded"))

;;; Load modules

(lightemacs--load-modules '(lightemacs))
(lightemacs--load-modules lightemacs-modules)

(minimal-emacs-load-user-init "config.el")

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
