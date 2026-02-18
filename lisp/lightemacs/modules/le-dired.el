;;; le-dired.el --- le-dired -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure `dired' to hide details such as file ownership and permissions, and
;; to group directories first.

;;; Code:

(require 'lightemacs-module)

(defvar lightemacs-dired-omit-parent-directory nil
  "When non-nil, omit the .. directory when `dired-omit-mode' is enabled.
The user can navigate to the parent directory by pressing the - key instead.")

(defvar lightemacs-dired-hide-details-mode t
  "Non-nil value enables `dired-hide-details-mode'.
When enabled, `dired' automatically hides file details such as permissions,
size, and modification dates.")

(defvar lightemacs-dired-group-directories-first t
  "If non-nil, Dired groups directories before files in listings.
When enabled, all directories are displayed at the beginning of the Dired
buffer, followed by regular files.")

(lightemacs-module-package dired
  :ensure nil
  :commands (dired
             dired-goto-file
             dired-find-file
             dired-get-marked-files
             dired-get-file-for-visit
             dired-guess-shell-command
             dired-hide-details-mode)

  :init
  (when lightemacs-dired-hide-details-mode
    ;; Dired buffers: Automatically hide file details (permissions, size,
    ;; modification date, etc.)
    (add-hook 'dired-mode-hook #'dired-hide-details-mode))

  ;; Group directories first
  (when lightemacs-dired-group-directories-first
    (when (string= system-type "darwin")
      (setq dired-use-ls-dired nil))
    (let ((args "--group-directories-first -ahlv"))
      (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
        (if-let* ((gls (executable-find "gls")))
            (setq insert-directory-program gls)
          (setq args nil)))
      (when args
        (setq dired-listing-switches args))))


  (setq dired-omit-files
        "\\`\\.[^.]")  ;; matches any file starting with . but not ..

  (setq dired-omit-files (concat
                          "\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                          "\\|\\(?:\\.js\\)?\\.meta\\'"
                          "\\|^\\.DS_Store\\'"
                          "\\|^\\.\\(?:svn\\|git\\|hg\\)\\'"
                          "\\|^\\.ccls-cache\\'"
                          "\\|^__pycache__\\'"
                          "\\|^\\.project\\(?:ile\\)?\\'"
                          "\\|^flycheck_.*"
                          "\\|^flymake_.*"))

  ;; TODO: Disable on Windows
  (if lightemacs-dired-omit-parent-directory
      ;; All dotfiles
      (setq dired-omit-files (concat dired-omit-files "\\|^\\."))
    ;; All dotfiles files except '..'
    (setq dired-omit-files (concat dired-omit-files "\\|\\`\\.[^.]\\|\\`\\.$"))))

(provide 'le-dired)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-dired.el ends here
