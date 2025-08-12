;;; mod-dired-filter.el --- mod-dired-filter -*- no-byte-compile: t; lexical-binding: t -*-

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

(use-package dired-filter
  :commands (dired-filter-by-git-ignored
             dired-filter-pop-all
             dired-filter-by-git-ignored
             dired-filter-by-dot-files
             dired-filter-by-omit)

  :hook
  (dired-mode . mod-dired-filter--setup)

  :init
  (defun mod-dired-filter--toggle-filter ()
    "Toggle the `dired' filter."
    (interactive)
    (when (boundp 'dired-filter-stack)
      (let ((dired-file (dired-get-file-for-visit)))
        (if (member '(omit) dired-filter-stack)
            (progn
              (dired-filter-pop-all))
          (dired-filter-pop-all)
          (dired-filter-by-omit)
          (dired-filter-by-git-ignored))

        (when dired-file
          (dired-goto-file dired-file)))))

  (defun mod-dired-filter--setup ()
    "Dired only hide didden files in ~/home"
    (dired-filter-by-omit)
    (dired-filter-by-git-ignored)
    (dired-filter-by-dot-files)))

(provide 'mod-dired-filter)

;;; mod-dired-filter.el ends here
