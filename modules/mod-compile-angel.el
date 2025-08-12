;;; mod-compile-angel.el --- Module: compile-angel -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The compile-angel package speeds up Emacs by ensuring that all Elisp
;; libraries are both byte-compiled and native-compiled:
;;
;; - Byte compilation reduces the overhead of loading Emacs Lisp code at
;;   runtime.
;; - Native compilation improves performance by generating machine code that
;;   runs directly on the hardware, leveraging the full capabilities of the host
;;   CPU. The actual speedup varies with the characteristics of the Lisp code,
;;   but it is typically 2.5 to 5 times faster than the equivalent byte-compiled
;;   version.
;;
;; URL: https://github.com/jamescherti/compile-angel.el

;;; Code:

(use-package compile-angel
  :demand t
  :commands (compile-angel-on-load-mode
             compile-angel-on-save-mode)

  :preface
  (defun mod-compile-angel-exclude (path)
    "Exclude PATH."
    (when (and (stringp path)
               (not (member path compile-angel-excluded-files)))
      (push (concat "/" (file-name-nondirectory path))
            compile-angel-excluded-files)))

  :config
  (setq compile-angel-verbose init-file-debug)
  (setq compile-angel-debug init-file-debug)

  (push "/org-version.el" compile-angel-excluded-files)

  (with-eval-after-load 'savehist
    (mod-compile-angel-exclude savehist-file))
  (with-eval-after-load 'recentf
    (mod-compile-angel-exclude recentf-save-file))
  (with-eval-after-load 'cus-edit
    (mod-compile-angel-exclude custom-file))
  (with-eval-after-load 'prescient
    (mod-compile-angel-exclude prescient-save-file))

  ;; A global mode that compiles .el files before they are loaded
  (compile-angel-on-load-mode))

(provide 'mod-compile-angel)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; mod-compile-angel.el ends here
