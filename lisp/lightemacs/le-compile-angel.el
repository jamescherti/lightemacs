;;; le-compile-angel.el --- le-compile-angel -*- no-byte-compile: t; lexical-binding: t -*-

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
  ;; :diminish compile-angel-on-load-mode
  :init
  (setq package-native-compile nil)

  ;; Verbose
  (setq compile-angel-verbose init-file-debug)
  (setq compile-angel-debug init-file-debug)

  ;; Enable `compile-angel-on-load-mode', a global mode that compiles .el files,
  ;; including those already loaded via `load' or `require' and those loaded
  ;; subsequently after the mode is activated.
  ;;
  ;; Since this uses `after-init-hook', it is necessary to ensure that
  ;; `compile-angel-on-load-compile-features' and
  ;; `compile-angel-on-load-compile-load-history' are both set to t.
  (setq compile-angel-on-load-compile-load-history t)
  (setq compile-angel-on-load-compile-features t)

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
  ;; Exclude files
  (with-eval-after-load 'org
    (push "/org-version.el" compile-angel-excluded-files))
  (with-eval-after-load 'savehist
    (le-compile-angel-exclude savehist-file))
  (with-eval-after-load 'recentf
    (le-compile-angel-exclude recentf-save-file))
  (with-eval-after-load 'cus-edit
    (le-compile-angel-exclude custom-file))
  (with-eval-after-load 'prescient
    (le-compile-angel-exclude prescient-save-file))

  ;; On load mode
  (compile-angel-on-load-mode))

(provide 'le-compile-angel)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-compile-angel.el ends here
