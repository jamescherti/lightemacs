;;; lem-dired-filter.el --- lem-dired-filter -*- lexical-binding: t -*-

;; Author: James Cherti
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

;; By default, `dired-filter-by-omit' excludes `"."`, which is generally
;; unnecessary.
(defvar lightemacs-dired-filter-setup-hook '(dired-filter-by-omit)
  "Hook of Dired filter functions to apply when entering `dired-mode'.

For instance:
  ;; Hide dotfiles
  (add-hook \\='lightemacs-dired-filter-setup-hook
            \\='dired-filter-by-dot-files)

  ;; Hide files that are ignored by Git
  (add-hook \\='lightemacs-dired-filter-setup-hook
            \\='dired-filter-by-git-ignored)")

(use-package dired-filter
  :commands (dired-filter-pop-all
             dired-filter-by-git-ignored
             dired-filter-by-git-ignored
             dired-filter-by-dot-files
             dired-filter-by-omit)

  :preface
  (defun lem-dired-filter--enable-filters ()
    "Dired only hide didden files in ~/home"
    (when (derived-mode-p 'dired-mode)
      (run-hooks 'lightemacs-dired-filter-setup-hook)))

  :init
  ;; Hide details such as file ownership and permissions
  (add-hook 'dired-mode-hook #'lem-dired-filter--enable-filters)

  ;; Toggle dired-filter
  (defun lem-dired-filter--toggle-filters ()
    "Toggle the `dired' filter."
    (interactive)
    (when (and (boundp 'dired-filter-stack)
               (fboundp 'dired-goto-file)
               (fboundp 'dired-get-file-for-visit))
      (let ((dired-file (dired-get-file-for-visit)))
        (dired-filter-pop-all)
        (unless (member '(omit) dired-filter-stack)
          (lem-dired-filter--enable-filters))

        (when dired-file
          (dired-goto-file dired-file))))))

(provide 'lem-dired-filter)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lem-dired-filter.el ends here
