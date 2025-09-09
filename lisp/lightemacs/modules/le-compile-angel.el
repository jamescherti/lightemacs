;;; le-compile-angel.el --- le-compile-angel -*- lexical-binding: t -*-

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

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package compile-angel
  :demand t
  :init
  ;; Verbose
  (setq compile-angel-verbose init-file-debug)
  (setq compile-angel-debug init-file-debug)

  :preface
  (defun le-compile-angel-exclude (path)
    "Add a file or directory to the list of exclusions for compilation.
PATH should be a string representing a file or directory path. If PATH is not
already present in `compile-angel-excluded-files', the basename of PATH,
prefixed with a forward slash, is appended to that list. This ensures that the
specified file or directory is ignored during the compilation process managed by
`compile-angel-on-load-mode'."
    (when (and (stringp path)
               (not (member path compile-angel-excluded-files)))
      (push (concat "/" (file-name-nondirectory path))
            compile-angel-excluded-files)))

  :config
  (push "/org-version.el" compile-angel-excluded-files)
  (push "/prescient-save.el" compile-angel-excluded-files)

  (with-eval-after-load 'savehist
    (le-compile-angel-exclude savehist-file))

  (with-eval-after-load 'recentf
    (le-compile-angel-exclude recentf-save-file))

  (with-eval-after-load 'cus-edit
    (le-compile-angel-exclude custom-file))

  ;; Start `compile-angel'
  (compile-angel-on-load-mode))

(provide 'le-compile-angel)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-compile-angel.el ends here
