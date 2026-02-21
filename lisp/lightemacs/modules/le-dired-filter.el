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

(require 'lightemacs-module)
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
             dired-filter-by-git-ignored
             dired-filter-by-dot-files
             dired-filter-by-omit)

  :bind (:map dired-mode-map
              ("C-c f" . lightemacs-dired-filter-toggle))

  :preface
  (defvar lightemacs--dired-filter-filters-enabled t)

  (defun lightemacs-dired-filter--enable-filters ()
    "Enable `dired' filters."
    (dired-filter-pop-all)  ; TODO check one by one using a loop
    (when (derived-mode-p 'dired-mode)
      (run-hooks 'lightemacs-dired-filter-setup-hook)))

  (defun lightemacs-dired-filter-toggle ()
    "Toggle `dired' filters on or off in the current buffer.

This function enables or disables all filters listed in
`lightemacs-dired-filter-setup-hook'. When toggled on, the filters are applied
to the current Dired buffer; when toggled off, all filters are removed,
restoring the full file listing."
    (interactive)
    (when (and (boundp 'dired-filter-stack)
               (fboundp 'dired-goto-file)
               (fboundp 'dired-get-file-for-visit))
      (let ((dired-file (dired-get-file-for-visit)))
        (dired-filter-pop-all)
        (if lightemacs--dired-filter-filters-enabled
            (setq lightemacs--dired-filter-filters-enabled nil)
          (setq lightemacs--dired-filter-filters-enabled t)
          (lightemacs-dired-filter--enable-filters))

        (when dired-file
          (dired-goto-file dired-file)))))

  :init
  (add-hook 'dired-mode-hook #'lightemacs-dired-filter--enable-filters))

(provide 'le-dired-filter)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-dired-filter.el ends here
