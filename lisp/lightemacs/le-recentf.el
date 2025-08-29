;;; le-recentf.el --- le-recentf -*- lexical-binding: t -*-

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

(eval-and-compile
  (require 'lightemacs))

;; Global variables

(defvar lightemacs-recentf-cleanup-and-auto-save-interval 550
  "Interval in seconds for running `recentf-cleanup' and `recentf-save-list'.")

(defvar lightemacs-recentf-track-switch-to-buffer t
  "Non-nil means track buffer switches and add the visited file to `recentf'.
When enabled, switching to a buffer visiting a file automatically
adds that file to the recentf list.")

(defvar lightemacs-recentf-quiet t
  "Non-nil means suppress messages during recentf cleanup and save operations.")

;; Local variables
(defvar lightemacs-recentf--auto-save-timer nil)

(lightemacs-use-package
  recentf
  :ensure nil
  :commands (recentf-mode
             recentf
             recentf-cleanup)
  :bind ("C-c f" . recentf)
  :init
  (defun lightemacs-recentf--cleanup ()
    "Run `recentf-cleanup' if `recentf-mode' is enabled."
    (when (and (bound-and-true-p recentf-mode)
               (fboundp 'recentf-cleanup))
      (let ((inhibit-message lightemacs-recentf-quiet))
        (recentf-cleanup))))

  (defun lightemacs-recentf--cleanup-and-save ()
    "Run `recentf-cleanup' and `recentf-save-list' if `recentf-mode' is enabled."
    ;; Cleanup
    (lightemacs-recentf--cleanup)
    ;; Save
    (when (and (bound-and-true-p recentf-mode)
               (fboundp 'recentf-save-list))
      (let ((inhibit-message lightemacs-recentf-quiet))
        (recentf-save-list))))

  (defun lightemacs-recentf--enable ()
    "Enable `recentf'."
    ;; Mode
    (let ((inhibit-message lightemacs-recentf-quiet))
      (recentf-mode 1))

    ;; Timer
    (when (timerp lightemacs-recentf--auto-save-timer)
      (cancel-timer lightemacs-recentf--auto-save-timer))
    (setq lightemacs-recentf--auto-save-timer
          (run-with-timer lightemacs-recentf-cleanup-and-auto-save-interval
                          lightemacs-recentf-cleanup-and-auto-save-interval
                          #'lightemacs-recentf--cleanup-and-save)))

  (defun lightemacs-recentf--disable ()
    "Disable `recentf' and cancel the auto-save timer."
    (when (timerp lightemacs-recentf--auto-save-timer)
      (cancel-timer lightemacs-recentf--auto-save-timer)
      (setq lightemacs-recentf--auto-save-timer nil))
    (recentf-mode -1))

  ;; Settings
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 750)
  (setq recentf-auto-cleanup 'never)  ; Managed by this module

  ;; Enable
  (add-hook 'lightemacs-on-first-buffer-hook #'lightemacs-recentf--enable)

  :config
  ;; Add file at the beginning of the recent list after switching buffer.
  (defun lightemacs-recentf--add-file-on-buffer-change (&rest _args)
    "Add file at the beginning of the recent list after switching buffer."
    (when (and (bound-and-true-p recentf-mode)
               (fboundp 'recentf-add-file))
      (when-let* ((file-name (buffer-file-name (buffer-base-buffer))))
        (recentf-add-file file-name))))

  (when lightemacs-recentf-track-switch-to-buffer
    (add-hook 'window-buffer-change-functions
              #'lightemacs-recentf--add-file-on-buffer-change))

  ;; Depth -90 ensures it is cleaned up before it is saved with
  ;; `recentf-save-list'
  (add-hook 'kill-emacs-hook #'lightemacs-recentf--cleanup -90))

(provide 'le-recentf)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-recentf.el ends here
