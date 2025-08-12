;;; mod-dired-filter.el --- mod-dired-filter -*- lexical-binding: t -*-

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

  :init
  ;; Hide details such as file ownership and permissions
  (defun mod-dired-filter--setup ()
    "Dired only hide didden files in ~/home"
    (dired-filter-by-omit)
    (dired-filter-by-git-ignored)
    (dired-filter-by-dot-files))
  (add-hook 'dired-mode-hook #'mod-dired-filter--setup)

  ;; Toggle dired-filter
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
          (dired-goto-file dired-file))))))

(provide 'mod-dired-filter)

;;; mod-dired-filter.el ends here
