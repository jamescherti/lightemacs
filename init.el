;;; init.el --- Init -*- lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The Lightemacs project is an Emacs framework.

;;; Code:

;;; Require

(require 'cl-lib)

;;; Load lightemacs.el

(eval-when-compile
  (defvar lightemacs-user-emacs-directory (file-truename "."))
  (defvar lightemacs--modules-dir (expand-file-name
                                   "lisp/lightemacs"
                                   lightemacs-user-emacs-directory))
  (add-to-list 'load-path lightemacs--modules-dir))

(eval-and-compile
  (require 'lightemacs))

;;; Load config.el

(load (expand-file-name "config" lightemacs-user-emacs-directory)
      nil  ; no-error
      (not (bound-and-true-p init-file-debug)))

;;; Package manager

(cond
 ;; Straight
 ((eq lightemacs-package-manager 'straight)
  (require 'le-core-straight))

 ;; Elpaca
 ((eq lightemacs-package-manager 'elpaca)
  (require 'le-core-elpaca))

 ;; use-package (built-in)
 ((eq lightemacs-package-manager 'use-package)
  t)

 (t
  (error (concat "Invalid value for `lightemacs-package-manager': '%s'. Valid "
                 "choices are: 'straight, 'elpaca, or 'use-package.")
         lightemacs-package-manager)))

;;; Init

(if (fboundp 'lightemacs-load-user-init)
    (progn
      ;; TODO move this before straight
      (if (fboundp 'minimal-emacs-load-user-init)
          (funcall 'minimal-emacs-load-user-init "pre-init.el")
        (error "The early-init.el file failed to loaded"))

      ;; TODO adjust this depending on the package manager
      (cl-letf (((symbol-function 'minimal-emacs-load-user-init)
                 (lambda (&rest _)
                   nil)))
        (lightemacs-load-user-init "init.el")))
  (error "The early-init.el file was not loaded"))

;; Initialize and refresh package contents again if needed
(when (and (not (bound-and-true-p lightemacs-package-manager))
           (not (eq lightemacs-package-manager 'straight)))
  (package-initialize))

;; Install use-package if necessary
(unless (package-installed-p 'use-package)
  (unless (seq-empty-p package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

;;; Load modules, and post-init.el

;; Load all modules
(if (fboundp 'lightemacs-load-modules)
    (funcall 'lightemacs-load-modules lightemacs-modules)
  (error "Undefined function: lightemacs-load-modules"))

;; Load post-init.el
(when (fboundp 'minimal-emacs-load-user-init)
  (funcall 'minimal-emacs-load-user-init "post-init.el"))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
