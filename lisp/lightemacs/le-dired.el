;;; le-dired.el --- le-dired -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure `dired' to hide details such as file ownership and permissions, and
;; to group directories first.

;;; Code:

(use-package dired
  :ensure nil
  :commands (dired
             dired-goto-file
             dired-find-file
             dired-get-marked-files
             dired-get-file-for-visit
             dired-guess-shell-command
             dired-hide-details-mode)

  :init
  ;; Dired buffers: Automatically hide file details (permissions, size,
  ;; modification date, etc.) and all the files in the `dired-omit-files'
  ;; regular expression for a cleaner display.
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (setq dired-omit-files (concat "\\`[.]\\'\\|^\\.git$"))

  ;; Other options
  (setq dired-movement-style 'bounded-files)

  ;; Group directories first
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (let ((args "--group-directories-first -ahlv"))
    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        (setq args nil)))
    (when args
      (setq dired-listing-switches args)))

  (setq dired-omit-files (concat "\\`[.]\\'"
                                 "\\|\\(?:\\.js\\)?\\.meta\\'"
                                 "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                                 "\\|^\\.DS_Store\\'"
                                 "\\|^\\.\\(?:svn\\|git\\|hg\\)\\'"
                                 "\\|^\\.ccls-cache\\'"
                                 "\\|^__pycache__\\'"
                                 "\\|^\\.project\\(?:ile\\)?\\'"
                                 "\\|^flycheck_.*"
                                 "\\|^flymake_.*"))
  (add-hook 'dired-mode-hook #'dired-omit-mode))

(provide 'le-dired)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-dired.el ends here
