;;; le-theme.el --- Module: tomorrow-night-deepblue-theme -*- lexical-binding: t -*-

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

(eval-and-compile
  (require 'lightemacs))

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

(defun lightemacs-theme--disable-all-themes (&optional exception)
  "Disable all themes currently listed in `custom-enabled-themes`.

EXCEPTION is the theme to exclude. If EXCEPTION is non-nil, it is excluded from
being disabled. Return non-nil if EXCEPTION remains enabled after the
operation."
  (interactive)
  (let ((exception-already-loaded nil))
    (mapc (lambda (theme)
            (when (or (not exception)
                      (eq theme exception))
              (disable-theme theme)))
          custom-enabled-themes)
    exception-already-loaded))

(defun lightemacs-load-default-theme (&optional force)
  "Load the theme defined in `lightemacs-theme-name' if it is available.
If the theme is not found in `custom-available-themes', a warning is issued.
If FORCE is non-nil, reload the current theme even if it is already active."
  (when (and lightemacs-theme-name
             (or force
                 (not (eq (car custom-enabled-themes) lightemacs-theme-name))))
    (if (memq lightemacs-theme-name (custom-available-themes))
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme lightemacs-theme-name t))
      (warn
       "[lightemacs] The theme '%s' is not available in the current environment"
       lightemacs-theme-name))))

;;; Main

(defvar le-theme--loaded nil)

(defun le-theme--load-theme ()
  "Load the default theme."
  (when (or (daemonp)
            (not le-theme--loaded))
    (unwind-protect
        (when-let* ((lightemacs-theme-name (if le-theme--loaded
                                               (car custom-enabled-themes)
                                             ;; Load the default one
                                             lightemacs-theme-name)))
          (lightemacs-load-default-theme t))
      (setq le-theme--loaded t))))

(when lightemacs-theme-package
  (eval `(lightemacs-use-package ,lightemacs-theme-package
           :demand t))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'le-theme--load-theme)
    (le-theme--load-theme)))

(provide 'le-theme)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-theme.el ends here
