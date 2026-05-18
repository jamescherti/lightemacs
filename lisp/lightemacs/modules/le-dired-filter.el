;;; le-dired-filter.el --- le-dired-filter -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure `dired' to hide dotfiles, omit files, and files listed in
;; '.gitignore'.
;;
;; URL: https://github.com/Fuco1/dired-hacks

;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'le-dired)

;;; Variables

;; `dired-filter-by-omit' is configured by le-dired. It not only hides
(defvar lightemacs-dired-filter-setup-hook '(dired-filter-by-omit)
  "Hook of Dired filter functions to apply when entering `dired-mode'.

For instance:
  ;; Hide files ignored by Git (may be slow in directories with many files)
  (add-hook \\='lightemacs-dired-filter-setup-hook
            \\='dired-filter-by-git-ignored)")

;;; Use-package dired-filter

(lightemacs-use-package dired-filter
  :commands (dired-filter-pop-all
             dired-filter-by-git-ignored
             dired-filter-by-dot-files
             dired-filter-by-omit)

  :bind (:map dired-mode-map
              ("C-c f" . lightemacs-dired-filter-local-toggle)
              ("C-c F" . lightemacs-dired-filter-global-toggle))

  :preface
  (defvar lightemacs--dired-filter-global-enabled nil
    "Global state for Dired filters.")

  (defvar-local lightemacs--dired-filter-local-enabled 'unspecified
    "Buffer-local state for Dired filters. Can be \='unspecified, t, or nil.")

  (defun lightemacs-dired-filter--enabled ()
    "Return t if filters should be enabled in the current buffer."
    (if (eq lightemacs--dired-filter-local-enabled 'unspecified)
        lightemacs--dired-filter-global-enabled
      lightemacs--dired-filter-local-enabled))

  (defun lightemacs-dired-filter--enable-filters ()
    "Enable `dired' filters based on user configuration."
    (dired-filter-pop-all)  ; TODO check one by one using a loop
    (when (lightemacs-dired-filter--enabled)
      (when (derived-mode-p 'dired-mode)
        (run-hooks 'lightemacs-dired-filter-setup-hook))))

  (defun lightemacs-dired-filter--apply-state ()
    "Save point, apply the filter state, and restore point."
    (when (and (boundp 'dired-filter-stack)
               (fboundp 'dired-goto-file)
               (fboundp 'dired-get-file-for-visit))
      (let ((dired-file (condition-case nil
                            (dired-get-file-for-visit)
                          (error nil))))
        (lightemacs-dired-filter--enable-filters)
        (when dired-file
          (dired-goto-file dired-file)))))

  (defun lightemacs-dired-filter-local-toggle ()
    "Toggle `dired' filters on or off in the current buffer.
This function enables or disables all filters listed in
`lightemacs-dired-filter-setup-hook'. When toggled on, the filters are applied
to the current Dired buffer; when toggled off, all filters are removed,
restoring the full file listing."
    (interactive)
    (setq lightemacs--dired-filter-local-enabled
          (not (lightemacs-dired-filter--enabled)))
    (lightemacs-dired-filter--apply-state)
    (message "Local Dired filters turned %s"
             (if (lightemacs-dired-filter--enabled) "on" "off")))

  (defun lightemacs-dired-filter-global-toggle ()
    "Toggle `dired' filters globally.
This applies the new global value to all open Dired buffers unless a specific
local value was set by the user."
    (interactive)
    (setq lightemacs--dired-filter-global-enabled
          (not lightemacs--dired-filter-global-enabled))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'dired-mode)
                   (eq lightemacs--dired-filter-local-enabled 'unspecified))
          (lightemacs-dired-filter--apply-state))))
    (when (called-interactively-p 'any)
      (message "Global Dired filters turned %s"
               (if lightemacs--dired-filter-global-enabled "on" "off"))))

  :init
  (add-hook 'dired-mode-hook #'lightemacs-dired-filter--enable-filters))

(provide 'le-dired-filter)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-dired-filter.el ends here
