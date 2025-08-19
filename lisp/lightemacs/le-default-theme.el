;;; le-default-theme.el --- Module: tomorrow-night-deepblue-theme -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The Emacs theme Tomorrow Night Deepblue is a beautiful deep blue variant of
;; the Tomorrow Night theme, which is renowned for its elegant color
;; palette that is pleasing to the eyes.
;;
;; The Tomorrow Night Deepblue features a deep blue background color that
;; creates a calming atmosphere. The contrasting colors make it easy to
;; distinguish between different elements of your code. The
;; tomorrow-night-deepblue theme is also a great choice for programmer who miss
;; the blue themes that were trendy a few years ago.
;;
;; The theme was inspired by classic text editors such as QuickBASIC, RHIDE, and
;; Turbo Pascal, as well as tools such as Midnight Commander which featured blue
;; backgrounds by default. There's something special about the early days of
;; programming and the tools we used that brings back fond memories.

;; URL: https://github.com/jamescherti/tomorrow-night-deepblue-theme.el

;;; Code:

(require 'lightemacs)

(cond
 ((eq lightemacs-theme 'tomorrow-night-deepblue)
  (use-package tomorrow-night-deepblue-theme
    :demand t
    :config
    (lightemacs-load-default-theme)))

 (t
  (when lightemacs-theme
    (lightemacs-load-default-theme))))

(provide 'le-default-theme)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-default-theme.el ends here
