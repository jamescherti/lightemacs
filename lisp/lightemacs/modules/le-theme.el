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

;;; Variables

;; Alternative:
;; - doom-one (Package: doom-themes)
(defvar lightemacs-theme-name 'ef-melissa-light
  "Default theme to load during initialization, if available.
Set to nil to disable loading a theme at startup.")

;; Alternative: doom-themes
(defvar lightemacs-theme-package 'ef-themes
  "Theme package to install and use for `lightemacs-theme-name'.
Set to nil to disable installing this package at startup.")

(defvar lightemacs-theme-default-font nil
  "Default font to apply after the theme is loaded.
Set to a string, such as \"Monospace-12\", or nil to keep the default font.")

;;; Theme

(defvar lightemacs-theme--package-installed nil)

(defun lightemacs-theme--apply (theme)
  "Apply THEME, disabling previously enabled themes."
  (if (memq theme (custom-available-themes))
      (let ((inhibit-redisplay t))
        (mapc #'disable-theme custom-enabled-themes)
        (condition-case err
            (load-theme theme t)
          (error (display-warning 'lightemacs
                                  (format "Failed to load theme '%s': %s"
                                          theme (error-message-string err))
                                  :warning))))
    (warn "[lightemacs] The theme '%s' is not available" theme)))

(defun lightemacs-load-default-theme (&optional force)
  "Load the theme defined in `lightemacs-theme-name' if it is available.
If the theme is not found in `custom-available-themes', a warning is issued.
If FORCE is non-nil, reload the current theme even if it is already active."
  (cond
   ((and lightemacs-theme-package
         lightemacs-theme-name
         (or force
             (not (eq (car custom-enabled-themes) lightemacs-theme-name))))
    (eval
     `(lightemacs-use-package ,lightemacs-theme-package
        :config
        (lightemacs-theme--apply ',lightemacs-theme-name)))
    ;; lexical-binding: t
    t)

   (lightemacs-theme-name
    (lightemacs-theme--apply lightemacs-theme-name))))

;;; Font
(defun lightemacs-theme-load-default-font (&rest _args)
  "Apply the default font defined in `lightemacs-theme-default-font'.
This function is idempotent and ignores _ARGS for `advice-add' compatibility."
  (when (and (display-graphic-p)
             lightemacs-theme-default-font)
    (let* ((get-fam (lambda (font)
                      (let* ((spec (font-spec :name (if (stringp font)
                                                        font
                                                      (font-xlfd-name font))))
                             (family (font-get spec :family)))
                        (cond
                         ((symbolp family) (symbol-name family))
                         ((stringp family) family)
                         (t nil)))))
           (current-family (funcall get-fam (frame-parameter nil 'font)))
           (target-family (funcall get-fam lightemacs-theme-default-font)))

      ;; Only apply if the family has changed to prevent UI flicker
      (unless (and current-family
                   target-family
                   (string-equal (downcase current-family)
                                 (downcase target-family)))
        (condition-case err
            (set-frame-font lightemacs-theme-default-font nil t :inhibit-customize)
          (error
           (display-warning 'lightemacs
                            (format "Font error: %s" (error-message-string err))
                            :warning)))))))

;;; Utility functions

(defun lightemacs-theme-create-loader (name &optional package)
  "Create an interactive function to load the Emacs theme specified by NAME.
If PACKAGE is non-nil, require it before loading the theme."
  (let* ((name-str (if (symbolp name) (symbol-name name) name))
         (theme-sym (intern name-str))
         (theme-fn-name (intern (format "color-%s" name-str)))
         (docstring (format "Load the `%s' Emacs theme." name-str)))
    (eval
     `(defun ,theme-fn-name ()
        ,docstring
        (interactive)
        (setq lightemacs-theme-package (if ',package
                                           ',package
                                         nil))
        (setq lightemacs-theme-name ',theme-sym)
        (lightemacs-load-default-theme t))
     t)))

;;; Main

(defvar lightemacs-theme--loaded nil)

(defun lightemacs-theme--load-theme ()
  "Load the default theme and font appropriately for GUI or TUI frames."
  ;; Load the theme colors (runs exactly once for the daemon lifecycle)
  ;;
  ;; Themes are global state: When `load-theme' evaluates, it modifies the
  ;; custom-enabled-themes variable and updates Emacs's internal registry of
  ;; face definitions (such as backgrounds, foregrounds, and syntax colors). The
  ;; headless daemon process retains this data in memory permanently, even when
  ;; zero client frames exist.
  (unless lightemacs-theme--loaded
    (lightemacs-load-default-theme t)
    (setq lightemacs-theme--loaded t))

  ;; Apply the font
  ;;
  ;; Fonts are tied to display capabilities: While a font can be set globally,
  ;; the process of creating a brand new X11 or Wayland window often prompts
  ;; Emacs to recalculate frame parameters based on system defaults. This is why
  ;; the font sometimes drops and needs to be reapplied, while the theme faces
  ;; persist.
  ;;
  ;; When you spawn a new client frame afterward, Emacs often resets the font
  ;; parameters.
  (when (daemonp)
    (lightemacs-theme-load-default-font)))

(unless noninteractive
  (advice-add 'load-theme :after #'lightemacs-theme-load-default-font)

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'lightemacs-theme--load-theme)
    (lightemacs-theme--load-theme)))

(provide 'le-theme)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-theme.el ends here
