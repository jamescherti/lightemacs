;;; le-recentf.el --- le-recentf -*- no-byte-compile: t; lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Recentf maintains a list of recently accessed files, making it easier to
;; reopen files you have worked on recently.
;;
;; In addition to its built-in capabilities, the le-recentf module provides the
;; following enhancements:
;; - Inserts the current file at the beginning of the recent files list upon
;;   buffer switch.
;; - Cleans up the recent files list when quitting Emacs, prior to its automatic
;;   saving.
;; - Decrease recentf-mode verbosity by restricting its messages to the
;;   *Messages* buffer, preventing display in the minibuffer

;;; Code:

(use-package recentf
  :ensure nil
  :commands (recentf-mode
             recentf-cleanup)
  :functions (recentf-expand-file-name)

  :preface
  (defun le-recentf--setup ()
    "Setup `recentf'."
    (let ((inhibit-message t))
      (recentf-mode)))

  (defun le-recentf--cleanup-and-save ()
    "Run `recentf-cleanup' if `recentf' is loaded and `recentf-mode' is enabled."
    (when (and (featurep 'recentf)
               (bound-and-true-p recentf-mode)
               (fboundp 'recentf-cleanup))
      (recentf-cleanup)))

  :init
  (add-hook 'lightemacs-on-first-buffer-hook #'le-recentf--setup)

  (setq recentf-auto-cleanup (if (daemonp) 300 'never))
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 750)

  :preface
  (defun le-recentf--track-buffer-change (&rest _args)
    "Add file at the beginning of the recent list after switching buffer."
    (when (and (bound-and-true-p recentf-mode)
               (fboundp 'recentf-add-file))
      (let ((file-name (buffer-file-name (buffer-base-buffer))))
        (when file-name
          (recentf-add-file file-name)))))

  :config
  (add-hook 'window-buffer-change-functions #'le-recentf--track-buffer-change)

  (setq recentf-exclude
        (append recentf-exclude
                (list
                 "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$" "\\.bz$"
                 "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zpaq$" "\\.lz$" "\\.lrz$"
                 "\\.lzo$" "\\.lzma$" "\\.shar$" "\\.kgb$" "\\.zip$" "\\.Z$"
                 "\\.7z$" "\\.rar$"

                 "COMMIT_EDITMSG\\'"
                 "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"

                 "-autoloads\\.el$"
                 "autoload\\.el$")))

  ;; Depth -90 ensures it is cleaned up before it is saved with
  ;; `recentf-save-list'
  (add-hook 'kill-emacs-hook #'le-recentf--cleanup-and-save -90))

(provide 'le-recentf)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-recentf.el ends here
