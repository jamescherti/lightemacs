;;; le-theme.el --- Module: tomorrow-night-deepblue-theme -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-use-package)

;; Variables

;; Alternative:
;; - doom-one (Package: doom-themes)
(defvar lightemacs-theme-name 'ef-melissa-light
  "Default theme to load during initialization, if available.
Set to nil to disable loading a theme at startup.")

;; Alternative: doom-themes
(defvar lightemacs-theme-package 'ef-themes
  "Theme package to install and use for `lightemacs-theme-name'.
Set to nil to disable installing this package at startup.")

;;; Functions

(defvar lightemacs-theme--package-installed nil)

(defun lightemacs-load-default-theme (&optional force)
  "Load the theme defined in `lightemacs-theme-name' if it is available.
If the theme is not found in `custom-available-themes', a warning is issued.
If FORCE is non-nil, reload the current theme even if it is already active."
  (unless (memq lightemacs-theme-name lightemacs-theme--package-installed)
    (eval `(lightemacs-use-package ,lightemacs-theme-package
             :demand t))
    (push lightemacs-theme-name lightemacs-theme--package-installed))

  (let ((lightemacs-theme-name lightemacs-theme-name))
    (when (and lightemacs-theme-name
               (or force
                   (not (eq (car custom-enabled-themes) lightemacs-theme-name))))

      (if (memq lightemacs-theme-name (custom-available-themes))
          (progn
            (mapc #'disable-theme custom-enabled-themes)
            (load-theme lightemacs-theme-name t))
        (warn "[lightemacs] The theme '%s' is not available"
              lightemacs-theme-name)))))

;;; Main

(defun le-theme--load-theme ()
  "Load the default theme."
  (lightemacs-load-default-theme t))

(when lightemacs-theme-package
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'le-theme--load-theme)
    (le-theme--load-theme)))

(provide 'le-theme)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-theme.el ends here
