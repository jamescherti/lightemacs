;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lightemacs is a Fast and Lightweight Emacs Framework.

;;; Code:

;;; Update defaults

;; This will be restored later by minimal-emacs.d
(setq load-prefer-newer t)

(setq minimal-emacs--backup-gc-cons-threshold gc-cons-threshold)
(defvar minimal-emacs--backup-gc-cons-percentage gc-cons-percentage)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

;; Defined later in le-core-defaults

(setq lightemacs-user-directory user-emacs-directory)
(setq lightemacs-local-directory (expand-file-name "lisp/local/"
                                                   lightemacs-user-directory))

;;; Load defaults

(add-to-list 'load-path (expand-file-name "lisp/lightemacs"
                                          user-emacs-directory))
(require 'le-core-defaults)

;;; Load: config.el

(load (expand-file-name "config" lightemacs-local-directory)
      :no-error
      (not (bound-and-true-p init-file-debug)))

;;; Function: `lightemacs-user-pre-early-init'

(when (fboundp 'lightemacs-user-pre-early-init)
  (funcall 'lightemacs-user-pre-early-init))

;;; Load: lightemacs.el

(require 'lightemacs)

;;; Load minimal-emacs.d early-init.el

(lightemacs-load-user-init
 (expand-file-name "early-init.el" minimal-emacs-user-directory))

;;; Adjust variables after early-init

(when (and lightemacs-native-comp-excluded-cpus
           (boundp 'native-comp-async-jobs-number))
  (setq native-comp-async-jobs-number
        (lightemacs--calculate-native-comp-async-jobs-number)))

(setq custom-file (expand-file-name "custom.el" lightemacs-var-directory))

;;; Function: `lightemacs-user-post-early-init'

(when (fboundp 'lightemacs-user-post-early-init)
  (funcall 'lightemacs-user-post-early-init))

;;; early-init.el ends here
