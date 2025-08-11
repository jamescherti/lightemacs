;;; mod-compile-angel.el --- Mod: compile-angel -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The compile-angel package speeds up Emacs by ensuring that all Elisp
;; libraries are both byte-compiled and native-compiled.

;;; Code:

(use-package compile-angel
  :demand t
  :commands (compile-angel-on-load-mode
             compile-angel-on-save-mode)
  :custom
  ;; Set `compile-angel-verbose' to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
  (defun mod-compile-angel-exclude (path)
    "Exclude STRING."
    (when (and (stringp path)
               (not (member path compile-angel-excluded-files)))
      (push (concat "/" (file-name-nondirectory path))
            compile-angel-excluded-files)))

  (setq compile-angel-verbose minimal-emacs-debug)
  (setq compile-angel-debug minimal-emacs-debug)
  (push "/org-version.el" compile-angel-excluded-files)
  (with-eval-after-load 'savehist
    (mod-compile-angel-exclude savehist-file))
  (with-eval-after-load 'recentf
    (mod-compile-angel-exclude recentf-save-file))
  (with-eval-after-load 'cus-edit
    (mod-compile-angel-exclude custom-file))
  (with-eval-after-load 'prescient
    (mod-compile-angel-exclude prescient-save-file))
  (push "/tmp-file.el" compile-angel-excluded-files)
  (push "/.dir-settings.el" compile-angel-excluded-files)

  ;; A global mode that compiles .el files before they are loaded.
  (compile-angel-on-load-mode))

(provide 'mod-compile-angel)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-compile-angel.el ends here
