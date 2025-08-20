;;; le-theme.el --- Module: tomorrow-night-deepblue-theme -*- no-byte-compile: t; lexical-binding: t -*-

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

;; Variables

(defvar lightemacs-theme-name 'doom-one
  "Default theme to load during initialization, if available.
Set to nil to disable loading a theme at startup.")

(defvar lightemacs-theme-package 'doom-themes
  "Theme package to install and use for `lightemacs-theme-name'.
Set to nil to disable installing this package at startup.")

;;; Functions

(defun lightemacs-load-default-theme ()
  "Load the theme defined in `lightemacs-theme-name' if it is installed."
  (when (and lightemacs-theme-name
             (member lightemacs-theme-name (custom-available-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme lightemacs-theme-name t)))

(defun lightemacs-theme-install ()
  "Install and configure `lightemacs-theme-package' using `use-package'."
  (when lightemacs-theme-package
    (eval `(use-package ,lightemacs-theme-package
             :demand t))))

;;; Main

;; Install theme
(lightemacs-theme-install)

;; Load theme
(when lightemacs-theme-name
  (lightemacs-load-default-theme))

(provide 'le-theme)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-theme.el ends here
