;;; le-recentf.el --- le-recentf -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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

(require 'lightemacs-module)

(require 'cl-lib)
(require 'le-shut-up)

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

(lightemacs-module-package recentf
  :ensure nil
  :commands (recentf-mode
             recentf
             recentf-cleanup
             recentf-save-list)
  :bind ("C-c f" . recentf)

  :preface
  (defun lightemacs-recentf--cleanup ()
    "Run `recentf-cleanup' if `recentf-mode' is enabled."
    (when (fboundp 'recentf-cleanup)
      (if lightemacs-recentf-quiet
          (shut-up
            (recentf-cleanup))
        (recentf-cleanup))))

  (defun lightemacs-recentf--save ()
    "Run `recentf-save-list' if `recentf-mode' is enabled."
    (when (fboundp 'recentf-save-list)
      (if lightemacs-recentf-quiet
          (shut-up
            (recentf-save-list))
        (recentf-save-list))))

  (defun lightemacs-recentf--cleanup-and-save ()
    "Run `recentf-cleanup' and `recentf-save-list' if `recentf-mode' is enabled."
    ;; Cleanup
    (lightemacs-recentf--cleanup)

    ;; Save
    (lightemacs-recentf--save))

  (defun lightemacs-recentf--enable ()
    "Enable `recentf'."
    ;; Mode
    (if lightemacs-recentf-quiet
        (shut-up
          (recentf-mode 1))
      (recentf-mode 1))

    ;; Replace `recentf-save-list' with a quiet version that cleans up and saves
    ;; the recentf list
    (remove-hook 'kill-emacs-hook 'recentf-save-list)
    (add-hook 'kill-emacs-hook #'lightemacs-recentf--cleanup-and-save)

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

  ;; Add file at the beginning of the recent list after switching buffer.
  (defun lightemacs-recentf--add-file-on-buffer-change (&rest _args)
    "Add file at the beginning of the recent list after switching buffer."
    (when (and (bound-and-true-p recentf-mode)
               (fboundp 'recentf-add-file))
      (when-let* ((file-name (buffer-file-name (buffer-base-buffer))))
        (recentf-add-file file-name))))

  :init
  ;; Settings
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 750)
  (setq recentf-auto-cleanup 'never)  ; Managed by this module

  ;; Enable
  (add-hook 'lightemacs-on-first-buffer-hook #'lightemacs-recentf--enable)

  :config
  (when lightemacs-recentf-track-switch-to-buffer
    (add-hook 'window-buffer-change-functions
              #'lightemacs-recentf--add-file-on-buffer-change)))

(provide 'le-recentf)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-recentf.el ends here
